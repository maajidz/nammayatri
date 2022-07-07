{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ExternalAPI.Flow where

import qualified Beckn.Types.Core.Metro.API.Search as MigAPI
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Core.Taxi.API.Cancel as API
import Beckn.Types.Core.Taxi.API.Confirm as API
import qualified Beckn.Types.Core.Taxi.API.Init as API
import Beckn.Types.Core.Taxi.API.Rating as API
import qualified Beckn.Types.Core.Taxi.API.Search as API
import Beckn.Types.Core.Taxi.API.Select as API
import Beckn.Utils.Dhall (FromDhall)
import Beckn.Utils.Error.BaseError.HTTPError.APIError
import Beckn.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Beckn.Utils.Servant.SignatureAuth
import EulerHS.Prelude
import qualified ExternalAPI.Types as API
import GHC.Records.Extra
import Servant.Client
import Tools.Metrics (CoreMetrics)
import Types.API.Location
import Types.Error
import Utils.Common

data BAPs a = BAPs
  { metro :: a,
    cabs :: a
  }
  deriving (Generic, FromDhall)

search ::
  ( HasField "gatewayUrl" r BaseUrl,
    MonadFlow m,
    CoreMetrics m,
    HasBapIds c r m
  ) =>
  API.SearchReq ->
  m API.SearchRes
search req = do
  url <- asks (.gatewayUrl)
  callBecknAPIWithSignature "search" API.searchAPI url req

searchMetro ::
  ( HasField "gatewayUrl" r BaseUrl,
    MonadFlow m,
    CoreMetrics m,
    HasBapIds c r m
  ) =>
  BecknReq MigAPI.SearchIntent ->
  m ()
searchMetro req = do
  url <- asks (.gatewayUrl)
  void $ callBecknAPIWithSignatureMetro "search" MigAPI.searchAPI url req

select ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapIds c r m
  ) =>
  BaseUrl ->
  SelectReq ->
  m SelectRes
select = callBecknAPIWithSignature "select" API.selectAPI

init ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapIds c r m
  ) =>
  BaseUrl ->
  API.InitReq ->
  m API.InitRes
init = callBecknAPIWithSignature "init" API.initAPI

confirm ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapIds c r m
  ) =>
  BaseUrl ->
  ConfirmReq ->
  m ConfirmRes
confirm = callBecknAPIWithSignature "confirm" API.confirmAPI

location ::
  ( MonadFlow m,
    CoreMetrics m
  ) =>
  BaseUrl ->
  Text ->
  m GetLocationRes
location url req = do
  -- TODO: fix authentication
  callOwnAPI Nothing Nothing url (API.location req) "location"
    `catchOwnAPI` throwError . \case
      "RIDE_INVALID_STATUS" -> RideInvalidStatus "Cannot track this ride"

cancel ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapIds c r m
  ) =>
  BaseUrl ->
  CancelReq ->
  m CancelRes
cancel = callBecknAPIWithSignature "cancel" API.cancelAPI

feedback ::
  ( MonadFlow m,
    CoreMetrics m,
    HasBapIds c r m
  ) =>
  BaseUrl ->
  RatingReq ->
  m RatingRes
feedback = callBecknAPIWithSignature "feedback" API.ratingAPI

type HasBapIds c r m =
  ( HasField "bapSelfIds" r (BAPs Text),
    MonadReader r m
  )

callBecknAPIWithSignature,
  callBecknAPIWithSignatureMetro ::
    ( MonadFlow m,
      CoreMetrics m,
      IsBecknAPI api req res,
      HasBapIds c r m
    ) =>
    Text ->
    Proxy api ->
    BaseUrl ->
    req ->
    m res
callBecknAPIWithSignature a b c d = do
  bapId <- asks (.bapSelfIds.cabs)
  callBecknAPI (Just $ getHttpManagerKey bapId) Nothing a b c d
callBecknAPIWithSignatureMetro a b c d = do
  bapId <- asks (.bapSelfIds.metro)
  callBecknAPI (Just $ getHttpManagerKey bapId) Nothing a b c d
