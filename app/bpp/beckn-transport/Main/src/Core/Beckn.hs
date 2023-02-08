{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Core.Beckn
  ( withCallback,
    withCallback',
    logBecknRequest,
  )
where

import Control.Lens.Operators ((?~))
import Data.List (lookup)
import qualified Data.Text.Encoding as T
import Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Queries.BecknRequest as QBR
import Kernel.Types.Id
import Kernel.Utils.Callback (WithBecknCallbackMig, withBecknCallbackMig)
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import qualified Network.Wai.Internal as Wai
import Servant
import SharedLogic.CallBAP (buildBppUrl)

withCallback ::
  HasFlowEnv m r '["nwAddress" ::: BaseUrl] =>
  DM.Merchant ->
  WithBecknCallbackMig api callback_success m
withCallback = withCallback' identity

withCallback' ::
  (m () -> m ()) ->
  HasFlowEnv m r '["nwAddress" ::: BaseUrl] =>
  DM.Merchant ->
  WithBecknCallbackMig api callback_success m
withCallback' doWithCallback transporter action api context cbUrl f = do
  let bppSubscriberId = getShortId $ transporter.subscriberId
      authKey = getHttpManagerKey bppSubscriberId
  bppUri <- buildBppUrl (transporter.id)
  let context' =
        context
          & #bpp_uri ?~ bppUri
          & #bpp_id ?~ bppSubscriberId
  withBecknCallbackMig doWithCallback (Just authKey) action api context' cbUrl f

logBecknRequest :: AppEnv -> Application -> Application
logBecknRequest appEnv f req@Wai.Request {..} respF = do
  req' <- case lookup "Authorization" requestHeaders of
    Nothing -> return req
    Just header -> do
      body <- requestBody
      bodyMvar <- newMVar body
      let logEnv = appEnv.loggerEnv
          esqDBEnv = appEnv.esqDBEnv
      Esq.runTransactionIO logEnv esqDBEnv $ do
        QBR.logBecknRequest (T.decodeUtf8 body) (T.decodeUtf8 header)
      return req {Wai.requestBody = mkRequestBody bodyMvar}
  f req' respF
  where
    mkRequestBody mvar = tryTakeMVar mvar <&> fromMaybe mempty
