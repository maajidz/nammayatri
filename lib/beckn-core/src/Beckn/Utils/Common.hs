{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Utils.Common where

import Beckn.Storage.DB.Config
import Beckn.TypeClass.IsError
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Types.Core.Error (Error (..))
import Beckn.Types.Error
import Beckn.Utils.Logging
import Beckn.Utils.Monitoring.Prometheus.Metrics as Metrics
import Control.Monad.Reader
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as DBB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Generics.Labels as GL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Data.Time
import Data.Time.Units (TimeUnit, fromMicroseconds)
import qualified EulerHS.Interpreters as I
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Runtime as R
import qualified EulerHS.Types as ET
import GHC.Records (HasField (..))
import GHC.TypeLits (Symbol)
import Network.HTTP.Types (Header, hContentType)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Types.Status
import Servant (ServerError (..), err500)
import qualified Servant.Client as S
import Servant.Client.Core.ClientError
import Servant.Client.Core.Response
import qualified Servant.Server.Internal as S

runFlowR :: R.FlowRuntime -> r -> FlowR r a -> IO a
runFlowR flowRt r x = I.runFlow flowRt . runReaderT x $ r

getCurrLocalTime :: L.MonadFlow m => m LocalTime
getCurrLocalTime = L.runIO $ do
  utc' <- getCurrentTime
  timezone <- getTimeZone utc'
  pure $ utcToLocalTime timezone utc'

getCurrTime :: L.MonadFlow m => m UTCTime
getCurrTime = L.runIO getCurrentTime

roundDiffTimeToUnit :: TimeUnit u => NominalDiffTime -> u
roundDiffTimeToUnit = fromMicroseconds . round . (* 1e6)

fromClientError :: ClientError -> Error
fromClientError err =
  Error
    { _type = "INTERNAL-ERROR",
      _code = "",
      _path = Nothing,
      _message = Just message
    }
  where
    message = case err of
      FailureResponse _ resp -> decodeUtf8 $ responseBody resp
      DecodeFailure _ resp -> decodeUtf8 $ responseBody resp
      UnsupportedContentType _ resp -> decodeUtf8 $ responseBody resp
      InvalidContentTypeHeader resp -> decodeUtf8 $ responseBody resp
      ConnectionError exc -> show exc

checkClientError :: (Log m, L.MonadFlow m) => Context -> Either S.ClientError a -> m a
checkClientError context = \case
  Right x -> pure x
  Left cliErr -> do
    let err = fromClientError cliErr
    logError "client call error" $ (err ^. #_message) ?: "Some error"
    L.throwException $ mkErrResponse context err500 err

-- | Get rid of database error
-- convert it into UnknownDomainError
checkDBError :: (HasCallStack, L.MonadFlow m, Log m) => ET.DBResult a -> m a
checkDBError dbres = checkDBError' dbres DatabaseError

-- | Get rid of database error
-- convert it into specified DomainError
-- f converts DBError to DomainError
checkDBError' ::
  (HasCallStack, L.MonadFlow m, Log m) =>
  ET.DBResult a ->
  (ET.DBError -> DomainError) ->
  m a
checkDBError' dbres f =
  case dbres of
    Left err -> throwDomainError $ f err
    Right res -> pure res

-- | Get rid of database error and empty result
-- convert it into UnknownDomainError
checkDBErrorOrEmpty ::
  (HasCallStack, L.MonadFlow m, Log m) =>
  ET.DBResult (Maybe a) ->
  DomainError ->
  m a
checkDBErrorOrEmpty dbres = checkDBErrorOrEmpty' dbres DatabaseError

-- | Get rid of database error and empty result
-- convert it into specified DomainError
-- f converts DBError to DomainError
checkDBErrorOrEmpty' ::
  (HasCallStack, L.MonadFlow m, Log m) =>
  ET.DBResult (Maybe a) ->
  (ET.DBError -> DomainError) ->
  DomainError ->
  m a
checkDBErrorOrEmpty' dbres f domainErrOnEmpty =
  case dbres of
    Left err -> throwDomainError $ f err
    Right maybeRes -> case maybeRes of
      Nothing -> throwDomainError domainErrOnEmpty
      Just x -> pure x

-- | Throw DomainError if DBError occurs
throwOnDBError :: (HasCallStack, L.MonadFlow m, Log m) => ET.DBResult a -> DomainError -> m a
throwOnDBError dbres domainError =
  checkDBError' dbres $ const domainError

-- Throw DomainErrors if DBError occurs or the result is empty
throwOnDBErrorOrEmpty ::
  (HasCallStack, L.MonadFlow m, Log m) =>
  ET.DBResult (Maybe a) ->
  DomainError ->
  DomainError ->
  m a
throwOnDBErrorOrEmpty dbres domainErrorOnDbError =
  checkDBErrorOrEmpty' dbres (const domainErrorOnDbError)

fromMaybeM :: (HasCallStack, L.MonadFlow m, Log m) => ServerError -> Maybe a -> m a
fromMaybeM err = maybe logAndThrow pure
  where
    logAndThrow = do
      logError "FROMMAYBE" (decodeUtf8 $ errBody err)
      L.throwException err

fromMaybeM400,
  fromMaybeM401,
  fromMaybeM404,
  fromMaybeM500,
  fromMaybeM503 ::
    (HasCallStack, L.MonadFlow m, Log m, IsError a APIError) => a -> Maybe b -> m b
fromMaybeM400 err = fromMaybeM (S.err400 {errBody = Aeson.encode @APIError $ toError err, errHeaders = [jsonHeader]})
fromMaybeM401 err = fromMaybeM (S.err401 {errBody = Aeson.encode @APIError $ toError err, errHeaders = [jsonHeader]})
fromMaybeM404 err = fromMaybeM (S.err404 {errBody = Aeson.encode @APIError $ toError err, errHeaders = [jsonHeader]})
fromMaybeM500 err = fromMaybeM (S.err500 {errBody = Aeson.encode @APIError $ toError err, errHeaders = [jsonHeader]})
fromMaybeM503 err = fromMaybeM (S.err503 {errBody = Aeson.encode @APIError $ toError err, errHeaders = [jsonHeader]})

fromMaybeMWithInfo400,
  fromMaybeMWithInfo401,
  fromMaybeMWithInfo404,
  fromMaybeMWithInfo500,
  fromMaybeMWithInfo503 ::
    (HasCallStack, L.MonadFlow m, Log m, IsError a APIError) => a -> Text -> Maybe b -> m b
fromMaybeMWithInfo400 err info = fromMaybeM (S.err400 {errBody = buildErrorBodyWithInfo err info, errHeaders = [jsonHeader]})
fromMaybeMWithInfo401 err info = fromMaybeM (S.err401 {errBody = buildErrorBodyWithInfo err info, errHeaders = [jsonHeader]})
fromMaybeMWithInfo404 err info = fromMaybeM (S.err404 {errBody = buildErrorBodyWithInfo err info, errHeaders = [jsonHeader]})
fromMaybeMWithInfo500 err info = fromMaybeM (S.err500 {errBody = buildErrorBodyWithInfo err info, errHeaders = [jsonHeader]})
fromMaybeMWithInfo503 err info = fromMaybeM (S.err503 {errBody = buildErrorBodyWithInfo err info, errHeaders = [jsonHeader]})

buildErrorBodyWithInfo :: (IsError a APIError) => a -> Text -> BSL.ByteString
buildErrorBodyWithInfo err info = Aeson.encode $ buildAPIErrorWithInfo err info

jsonHeader :: (HeaderName, ByteString)
jsonHeader = (hContentType, "application/json;charset=utf-8")

mkOkResponse :: L.MonadFlow m => Context -> m AckResponse
mkOkResponse context = do
  currTime <- getCurrTime
  let context' = context {_timestamp = currTime}
  return $ AckResponse context' (ack "ACK") Nothing

mkAckResponse :: L.MonadFlow m => Text -> Text -> m AckResponse
mkAckResponse txnId action = mkAckResponse' txnId action "ACK"

mkAckResponse' :: L.MonadFlow m => Text -> Text -> Text -> m AckResponse
mkAckResponse' txnId action status = do
  currTime <- getCurrTime
  return
    AckResponse
      { _context =
          Context
            { _domain = MOBILITY,
              _country = Just "IND",
              _city = Nothing,
              _action = action,
              _core_version = Nothing,
              _domain_version = Nothing,
              _bap_uri = Nothing,
              _bpp_uri = Nothing,
              _transaction_id = txnId,
              _message_id = txnId,
              _timestamp = currTime,
              _ttl = Nothing
            },
        _message =
          ack status,
        _error = Nothing
      }

mkErrResponse :: Context -> ServerError -> Error -> NackResponseError
mkErrResponse context errBase err =
  NackResponseError
    { _context = context,
      _error = err,
      _status = mkStatus (errHTTPCode errBase) (encodeUtf8 $ errReasonPhrase errBase)
    }

compileErrResponse :: NackResponseError -> AckResponse
compileErrResponse NackResponseError {..} =
  AckResponse
    { _context = _context,
      _message = ack "NACK",
      _error = Just _error
    }

withFlowHandler :: FlowR r a -> FlowHandlerR r a
withFlowHandler flow = do
  (EnvR flowRt appEnv) <- ask
  lift . ExceptT . try . runFlowR flowRt appEnv $ flow

decodeFromText :: FromJSON a => Text -> Maybe a
decodeFromText = A.decode . BSL.fromStrict . DT.encodeUtf8

encodeToText :: ToJSON a => a -> Text
encodeToText = DT.decodeUtf8 . BSL.toStrict . A.encode

-- strips double quotes from encoded text
encodeToText' :: ToJSON a => a -> Text
encodeToText' s =
  let s' = A.encode s
   in if BSL.length s' < 2
        then DT.decodeUtf8 $ BSL.toStrict s'
        else DT.decodeUtf8 $ BSL.toStrict $ BSL.tail $ BSL.init s'

authenticate ::
  ( HasField "cronAuthKey" r (Maybe CronAuthKey),
    HasLogContext r
  ) =>
  Maybe CronAuthKey ->
  FlowR r ()
authenticate = check handleKey
  where
    handleKey rauth = do
      key <- check pure =<< getField @"cronAuthKey" <$> ask
      check (flip when throw401 . (key /=)) $
        DT.decodeUtf8 <$> (rightToMaybe . DBB.decode . DT.encodeUtf8 =<< T.stripPrefix "Basic " rauth)
    check = maybe throw401
    throw401 :: HasLogContext r => FlowR r a
    throw401 =
      throwError401 AuthBlocked

throwHttpError :: forall e m a. (HasCallStack, L.MonadFlow m, Log m, ToJSON e) => ServerError -> e -> m a
throwHttpError err errMsg = do
  let body = Aeson.encode errMsg
  logError "HTTP_ERROR" (decodeUtf8 body)
  L.throwException err {errBody = body, errHeaders = [jsonHeader]}

throwBecknError :: (HasCallStack, L.MonadFlow m, Log m) => ServerError -> Text -> m a
throwBecknError err errMsg = do
  logError "Beckn error" errMsg
  L.throwException
    err
      { errBody = A.encode $ getBecknError err errMsg,
        errHeaders = jsonHeader : errHeaders err
      }

getBecknError :: S.ServerError -> Text -> BecknError
getBecknError err msg =
  BecknError
    { _errorCode = ErrorCode $ errHTTPCode err,
      _errorMessage = ErrorMsg msg,
      _action = NACK
    }

throwError500,
  throwError501,
  throwError503,
  throwError400,
  throwError401,
  throwError403,
  throwError404 ::
    (HasCallStack, L.MonadFlow m, Log m, IsError a APIError) => a -> m b
throwError500 = throwHttpError @APIError S.err500 . toError
throwError501 = throwHttpError @APIError S.err501 . toError
throwError503 = throwHttpError @APIError S.err503 . toError
throwError400 = throwHttpError @APIError S.err400 . toError
throwError401 = throwHttpError @APIError S.err401 . toError
throwError403 = throwHttpError @APIError S.err403 . toError
throwError404 = throwHttpError @APIError S.err404 . toError

throwErrorWithInfo500,
  throwErrorWithInfo501,
  throwErrorWithInfo503,
  throwErrorWithInfo400,
  throwErrorWithInfo401,
  throwErrorWithInfo403,
  throwErrorWithInfo404 ::
    (HasCallStack, L.MonadFlow m, Log m, IsError a APIError) => a -> Text -> m b
throwErrorWithInfo500 err info = throwHttpError @APIError S.err500 $ buildAPIErrorWithInfo err info
throwErrorWithInfo501 err info = throwHttpError @APIError S.err501 $ buildAPIErrorWithInfo err info
throwErrorWithInfo503 err info = throwHttpError @APIError S.err503 $ buildAPIErrorWithInfo err info
throwErrorWithInfo400 err info = throwHttpError @APIError S.err400 $ buildAPIErrorWithInfo err info
throwErrorWithInfo401 err info = throwHttpError @APIError S.err401 $ buildAPIErrorWithInfo err info
throwErrorWithInfo403 err info = throwHttpError @APIError S.err403 $ buildAPIErrorWithInfo err info
throwErrorWithInfo404 err info = throwHttpError @APIError S.err404 $ buildAPIErrorWithInfo err info

buildAPIErrorWithInfo :: (IsError a APIError) => a -> Text -> APIError
buildAPIErrorWithInfo err info =
  let apiErr@APIError {..} = toError err
      defMsg = fromMaybe "" errorMessage
   in apiErr
        { errorMessage = Just $ defMsg <> " " <> info
        }

throwBecknError500,
  throwBecknError501,
  throwBecknError400,
  throwBecknError401,
  throwBecknError404 ::
    (HasCallStack, L.MonadFlow m, Log m) => Text -> m a
throwBecknError500 = throwBecknError S.err500
throwBecknError501 = throwBecknError S.err501
throwBecknError400 = throwBecknError S.err400
throwBecknError401 = throwBecknError S.err401
throwBecknError404 = throwBecknError S.err404

throwAuthError :: (HasCallStack, L.MonadFlow m, Log m) => [Header] -> Text -> m a
throwAuthError headers = throwBecknError (S.err401 {errHeaders = headers})

-- | Format time in IST and return it as text
-- Converts and Formats in the format
-- TODO: make a generic function and then pass format
-- and timezone as arguments. Currently adds +5:30
showTimeIst :: UTCTime -> Text
showTimeIst = T.pack . formatTime defaultTimeLocale "%d %b, %I:%M %p" . addUTCTime (60 * 330)

throwDomainError :: (HasCallStack, L.MonadFlow m, Log m) => DomainError -> m a
throwDomainError err =
  case err of
    UnknownDomainError msg -> t S.err500 msg
    -- TODO get more details from db error?
    DatabaseError (ET.DBError _ text) -> t S.err500 $ show text
    -- Case errors
    CaseErr suberr -> case suberr of
      CaseNotFound -> t S.err404 "Case not found"
      CaseStatusTransitionErr msg -> t S.err405 msg
      CaseNotCreated -> t S.err404 "Case not created"
      CaseNotUpdated -> t S.err404 "Case not updated"
      _ -> t S.err404 "Case not updated"
    -- Product Instance errors
    ProductInstanceErr suberr -> case suberr of
      ProductInstanceNotFound -> t S.err404 "Product Instance not found"
      ProductInstanceStatusTransitionErr msg -> t S.err405 msg
      _ -> t S.err404 "Case not updated"
    -- Product errors
    ProductErr suberr -> case suberr of
      ProductNotFound -> t S.err404 "Product not found"
      ProductNotUpdated -> t S.err405 "Product not updated"
      ProductNotCreated -> t S.err405 "Product not created"
      _ -> t S.err404 "Case not updated"
    AuthErr Unauthorized -> t S.err401 "Unauthorized"
    _ -> t S.err500 "Unknown error"
  where
    t errCode (ErrorMsg errMsg) = throwBecknError errCode errMsg

callClient ::
  (ET.JSONEx a, L.MonadFlow m, Log m) =>
  Text ->
  Context ->
  S.BaseUrl ->
  ET.EulerClient a ->
  m a
callClient = callClient' Nothing

-- TODO: the @desc@ argument should become part of monadic context
callClient' ::
  (ET.JSONEx a, L.MonadFlow m, Log m) =>
  Maybe String ->
  Text ->
  Context ->
  S.BaseUrl ->
  ET.EulerClient a ->
  m a
callClient' mbManager desc context baseUrl cli = do
  endTracking <- L.runIO $ Metrics.startTracking (encodeToText' baseUrl) desc
  res <- L.callAPI' mbManager baseUrl cli
  _ <- L.runIO $ endTracking $ getResponseCode res
  case res of
    Left err -> do
      logError "cli" $ "Failure in " <> show desc <> " call to " <> toText (S.showBaseUrl baseUrl) <> ": " <> show err
      L.throwException $ mkErrResponse context err500 (fromClientError err)
    Right x -> pure x
  where
    getResponseCode res =
      case res of
        Right _ -> "200"
        Left (FailureResponse _ (Response code _ _ _)) -> T.pack $ show code
        Left (DecodeFailure _ (Response code _ _ _)) -> T.pack $ show code
        Left (InvalidContentTypeHeader (Response code _ _ _)) -> T.pack $ show code
        Left (UnsupportedContentType _ (Response code _ _ _)) -> T.pack $ show code
        Left (ConnectionError _) -> "Connection error"

-- | A replacement for 'L.forkFlow' which works in 'FlowR'.
-- It's main use case is to perform an action asynchronously without waiting for
-- result.
--
-- It has several differences comparing to 'L.forkFlow':
-- * Logs errors in case if the action failed;
-- * Expects action to return '()' - this is good, because the opposite means
--   you ignored something important, e.g. an exception returned explicitly;
-- * Do not log the fact of thread creation (was it any useful?)
--
-- NOTE: this function is temporary, use of bare forking is bad and should be
-- removed one day.

-- I know it is looking similar to forkAsync but I found it simpler to
-- be able to use (FlowR r) instead of (FlowR (EnvR r))
fork :: HasLogContext r => Text -> FlowR r () -> FlowR r ()
fork desc f = do
  env <- ask
  lift $ L.forkFlow desc $ handleExc env $ runReaderT f env
  where
    handleExc env a =
      L.runSafeFlow a >>= \case
        Right () -> pass
        Left e -> runReaderT (err e) env
    err e =
      logWarning "Thread" $
        "Thread " <> show desc <> " died with error: " <> show e

runSafeFlow :: (FromJSON a, ToJSON a) => FlowR r a -> FlowR r (Either Text a)
runSafeFlow flow = do
  env <- ask
  lift $ L.runSafeFlow $ runReaderT flow env

addIfPresent :: [a] -> Maybe a -> [a]
addIfPresent xs (Just x) = x : xs
addIfPresent xs _ = xs

isExpired :: L.MonadFlow m => NominalDiffTime -> UTCTime -> m Bool
isExpired nominal time = do
  now <- getCurrTime
  let addedUTCTime = addUTCTime nominal time
  return $ now > addedUTCTime

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

class HasSchemaName m where
  getSchemaName :: m Text

instance HasDbCfg r => HasSchemaName (FlowR r) where
  getSchemaName =
    asks (schemaName <$> getField @"dbCfg")

-- | Get trace flag from ENV var
getTraceFlag :: HasTraceFlag r => FlowR r TraceFlag
getTraceFlag =
  getField @"traceFlag" <$> ask

padLeft :: Int -> Char -> Text -> Text
padLeft n c txt =
  let prefix = replicate (max 0 $ n - length txt) c
   in T.pack prefix <> txt

-- Suits only for non-negative numbers
padNumber :: Integral i => Int -> i -> Text
padNumber n num = padLeft n '0' $ show (fromIntegral num :: Natural)

-- | An alias for type-level pair of name and type.
type (name :: Symbol) ::: (ty :: Type) = '(name, ty)

-- | Version of 'HasField' which complies with both record-dot-preprocessor
-- and @^. #field@ syntax supported by generics-lens.
--
-- Re-evaluate this once we decide on a uniform way to access fields.
type HasFieldSuper name r ty = (HasField name r ty, GL.Field name r r ty ty)

-- | Bulk version of @HasField@.
type family HasFields (r :: Type) (fields :: [(Symbol, Type)]) :: Constraint where
  HasFields r '[] = () :: Constraint
  HasFields r ('(name, ty) ': fields) =
    (HasFieldSuper name r ty, HasFields r fields)

-- | Require monad to be Flow-based and have specified fields in Reader env.
type HasFlowEnv m r fields =
  ( L.MonadFlow m,
    MonadReader r m,
    HasFields r fields,
    HasLogContext r
  )

foldWIndex :: (Integer -> acc -> a -> acc) -> acc -> [a] -> acc
foldWIndex f acc p = snd $ foldl (\(i, acc') c -> (i + 1, f i acc' c)) (0, acc) p

addLogTag :: HasLogContext env => Text -> FlowR env a -> FlowR env a
addLogTag = local . addLogTagToEnv
