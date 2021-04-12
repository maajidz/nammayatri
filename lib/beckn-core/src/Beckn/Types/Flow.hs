{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Types.Flow (FlowR) where

import Beckn.Utils.Logging
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Prelude (show)

type FlowR r = ReaderT r L.Flow

instance Log (FlowR r) where
  logOutput logLevel message =
    case logLevel of
      DEBUG -> L.logDebug EmtpyTag message
      INFO -> L.logInfo EmtpyTag message
      WARNING -> L.logWarning EmtpyTag message
      ERROR -> L.logError EmtpyTag message
  withLogContext lc flowR =
    let f = runReaderT flowR
     in ReaderT $ \v -> L.withLoggerContext (appendLogContext lc) $ f v

data EmtpyTag = EmtpyTag

instance Show EmtpyTag where
  show _ = ""