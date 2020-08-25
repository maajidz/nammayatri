{-# LANGUAGE TypeApplications #-}

module Beckn.Utils.Common where

import Beckn.Storage.DB.Config
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Core.Domain
import Beckn.Types.Error
import Data.Aeson as A
import qualified Data.ByteString.Base64 as DBB
import qualified Data.ByteString.Lazy as BSL
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
import Network.HTTP.Types (hContentType)
import Servant
import Servant.Client (BaseUrl)

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

-- | Get rid of database error
-- convert it into UnknownDomainError
checkDBError :: L.MonadFlow m => ET.DBResult a -> m a
checkDBError dbres = checkDBError' dbres DatabaseError

-- | Get rid of database error
-- convert it into specified DomainError
-- f converts DBError to DomainError
checkDBError' :: L.MonadFlow m => ET.DBResult a -> (ET.DBError -> DomainError) -> m a
checkDBError' dbres f =
  case dbres of
    Left err -> throwDomainError $ f err
    Right res -> pure res

-- | Get rid of database error and empty result
-- convert it into UnknownDomainError
checkDBErrorOrEmpty :: L.MonadFlow m => ET.DBResult (Maybe a) -> DomainError -> m a
checkDBErrorOrEmpty dbres = checkDBErrorOrEmpty' dbres DatabaseError

-- | Get rid of database error and empty result
-- convert it into specified DomainError
-- f converts DBError to DomainError
checkDBErrorOrEmpty' :: L.MonadFlow m => ET.DBResult (Maybe a) -> (ET.DBError -> DomainError) -> DomainError -> m a
checkDBErrorOrEmpty' dbres f domainErrOnEmpty =
  case dbres of
    Left err -> throwDomainError $ f err
    Right maybeRes -> case maybeRes of
      Nothing -> throwDomainError domainErrOnEmpty
      Just x -> pure x

-- | Throw DomainError if DBError occurs
throwOnDBError :: L.MonadFlow m => ET.DBResult a -> DomainError -> m a
throwOnDBError dbres domainError =
  checkDBError' dbres $ const domainError

-- Throw DomainErrors if DBError occurs or the result is empty
throwOnDBErrorOrEmpty :: L.MonadFlow m => ET.DBResult (Maybe a) -> DomainError -> DomainError -> m a
throwOnDBErrorOrEmpty dbres domainErrorOnDbError =
  checkDBErrorOrEmpty' dbres (const domainErrorOnDbError)

fromMaybeM :: L.MonadFlow m => ServerError -> Maybe a -> m a
fromMaybeM err Nothing = L.throwException err
fromMaybeM _ (Just a) = return a

fromMaybeM400,
  fromMaybeM401,
  fromMaybeM500,
  fromMaybeM503 ::
    L.MonadFlow m => BSL.ByteString -> Maybe a -> m a
fromMaybeM400 a = fromMaybeM (err400 {errBody = a})
fromMaybeM401 a = fromMaybeM (err401 {errBody = a})
fromMaybeM500 a = fromMaybeM (err500 {errBody = a})
fromMaybeM503 a = fromMaybeM (err503 {errBody = a})

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
              _country = Nothing,
              _city = Nothing,
              _action = action,
              _core_version = Nothing,
              _domain_version = Nothing,
              _bap_uri = Nothing,
              _bpp_uri = Nothing,
              _transaction_id = txnId,
              _message_id = txnId,
              _timestamp = currTime
            },
        _message =
          ack status,
        _error = Nothing
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
  let s' = encode s
   in if BSL.length s' < 2
        then DT.decodeUtf8 $ BSL.toStrict s'
        else DT.decodeUtf8 $ BSL.toStrict $ BSL.tail $ BSL.init s'

authenticate :: HasField "cronAuthKey" r (Maybe CronAuthKey) => Maybe CronAuthKey -> FlowR r ()
authenticate = check handleKey
  where
    handleKey rauth = do
      key <- check pure =<< getField @"cronAuthKey" <$> ask
      check (flip when throw401 . (key /=)) $
        DT.decodeUtf8 <$> (rightToMaybe . DBB.decode . DT.encodeUtf8 =<< T.stripPrefix "Basic " rauth)
    check = maybe throw401
    throw401 =
      L.throwException $
        err401 {errBody = "Invalid Auth"} ::
        FlowR r a

throwJsonError :: L.MonadFlow m => ServerError -> Text -> Text -> m a
throwJsonError err tag errMsg = do
  L.logError tag errMsg
  L.throwException
    err
      { errBody = A.encode $ getBecknError err errMsg,
        errHeaders = pure jsonHeader
      }
  where
    jsonHeader =
      ( hContentType,
        "application/json;charset=utf-8"
      )

getBecknError :: ServerError -> Text -> BecknError
getBecknError err msg =
  BecknError
    { _errorCode = ErrorCode $ errHTTPCode err,
      _errorMessage = ErrorMsg msg,
      _action = NACK
    }

throwJsonError500,
  throwJsonError501,
  throwJsonError400,
  throwJsonError401,
  throwJsonError404 ::
    L.MonadFlow m => Text -> Text -> m a
throwJsonError500 = throwJsonError err500
throwJsonError501 = throwJsonError err501
throwJsonError400 = throwJsonError err400
throwJsonError401 = throwJsonError err401
throwJsonError404 = throwJsonError err404

throwJsonErrorH :: ServerError -> Text -> Text -> FlowHandlerR r a
throwJsonErrorH = withFlowHandler ... throwJsonError

throwJsonError500H, throwJsonError501H, throwJsonError400H, throwJsonError401H :: Text -> Text -> FlowHandlerR r a
throwJsonError500H = throwJsonErrorH ... err500
throwJsonError501H = throwJsonErrorH ... err501
throwJsonError400H = throwJsonErrorH ... err400
throwJsonError401H = throwJsonErrorH ... err401

-- | Format time in IST and return it as text
-- Converts and Formats in the format
-- TODO: make a generic function and then pass format
-- and timezone as arguments. Currently adds +5:30
showTimeIst :: UTCTime -> Text
showTimeIst = T.pack . formatTime defaultTimeLocale "%d %b, %I:%M %p" . addUTCTime (60 * 330)

throwDomainError :: L.MonadFlow m => DomainError -> m a
throwDomainError err =
  case err of
    UnknownDomainError msg -> t err401 msg
    -- TODO get more details from db error?
    DatabaseError (ET.DBError _ text) -> t err500 $ show text
    -- Case errors
    CaseErr suberr -> case suberr of
      CaseNotFound -> t err404 "Case not found"
      CaseStatusTransitionErr msg -> t err405 msg
      CaseNotCreated -> t err404 "Case not created"
      CaseNotUpdated -> t err404 "Case not updated"
    -- Product Instance errors
    ProductInstanceErr suberr -> case suberr of
      ProductInstanceNotFound -> t err404 "Product Instance not found"
      ProductInstanceStatusTransitionErr msg -> t err405 msg
    -- Product errors
    ProductErr suberr -> case suberr of
      ProductNotFound -> t err404 "Product not found"
      ProductNotUpdated -> t err405 "Product not updated"
      ProductNotCreated -> t err405 "Product not created"
    AuthErr UnAuthorized -> t err401 "Unauthorized"
    _ -> t err500 "Unknown error"
  where
    t errCode (ErrorMsg errMsg) = throwJsonError errCode "error" errMsg

-- TODO: the @desc@ argument should become part of monadic context
callClient ::
  (ET.JSONEx a, L.MonadFlow m) =>
  Text ->
  BaseUrl ->
  ET.EulerClient a ->
  m a
callClient desc baseUrl cli =
  L.callAPI baseUrl cli >>= \case
    Left err -> do
      L.logError @Text "cli" $ "Failure in " <> show desc <> " call to " <> show baseUrl <> ": " <> show err
      L.throwException err
    Right x -> pure x

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
forkAsync :: Text -> FlowR (EnvR r) () -> FlowR (EnvR r) ()
forkAsync desc action =
  void . ReaderT $ \env@(EnvR flowRt _) ->
    L.runUntracedIO . forkIO $
      I.runFlow flowRt (runReaderT action env)
        `catchAny` \e -> I.runFlow flowRt (logErr e)
  where
    logErr e =
      L.logWarning @Text "Thread" $
        "Thread " <> show desc <> " died with error: " <> show e

-- I know it is looking similar to forkAsync but I found it simpler to
-- be able to use (FlowR r) instead of (FlowR (EnvR r))
-- NOTE: Silently fails in case of any errors thrown inside Flow.
-- You will have to explicitly use log for debugging
fork :: Text -> FlowR r () -> FlowR r ()
fork desc f = do
  env <- ask
  lift $ L.forkFlow desc $ runReaderT f env

class Example a where
  -- | Sample value of a thing.
  --
  -- This can be used for mocking.
  -- Also, it is especially useful for including examples into swagger,
  -- because random generation can produce non-demostrative values
  -- (e.g. empty lists) unless special care is taken.
  example :: a

instance Example a => Example (Maybe a) where
  example = Just example

instance Example a => Example [a] where
  example = one example

instance Example UTCTime where
  example =
    UTCTime
      (fromGregorian 2020 8 2)
      (timeOfDayToTime (TimeOfDay 7 7 7))

-- until we start using newtypes everywhere
idExample :: Text
idExample = "123e4567-e89b-12d3-a456-426655440000"

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

getSchemaName :: HasDbCfg r => FlowR r Text
getSchemaName =
  schemaName <$> getField @"dbCfg" <$> ask

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
