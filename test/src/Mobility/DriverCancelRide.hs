module Mobility.DriverCancelRide where

import qualified "beckn-transport" API.UI.Booking as TbeBookingAPI
import qualified "beckn-transport" API.UI.Ride as RideAPI
import Common (getAppBaseUrl)
import qualified "app-backend" Domain.Types.Booking as AppRB
import qualified "beckn-transport" Domain.Types.Booking as TRB
import qualified "beckn-transport" Domain.Types.CancellationReason as SCR
import qualified "beckn-transport" Domain.Types.Ride as TRide
import EulerHS.Prelude
import HSpec
import Mobility.Fixtures
import Mobility.SuccessFlow
import Utils

spec :: Spec
spec = do
  clients <- runIO $ mkMobilityClients getAppBaseUrl getTransporterBaseUrl
  describe "Testing App and Transporter APIs" $
    it "Testing API flow for ride cancelled by Driver" $ withBecknClients clients do
      void . callBPP $ setDriverOnline driverToken2 True
      bBookingId <- doAnAppSearch

      tBooking <- poll $ do
        trb <- getBPPBooking bBookingId
        trb.status `shouldBe` TRB.CONFIRMED
        return $ Just trb

      rideInfo <-
        poll . callBPP $
          getNotificationInfo tBooking.id driverToken1
            <&> (.rideRequest)
      rideInfo.bookingId `shouldBe` tBooking.id

      -- Driver1 Accepts a ride
      void . callBPP $
        rideRespond tBooking.id driverToken1 $
          TbeBookingAPI.SetDriverAcceptanceReq TbeBookingAPI.ACCEPT

      tRide1 <- poll $ do
        tRide <- getBPPRide tBooking.id
        tRide.status `shouldBe` TRide.NEW
        return $ Just tRide

      void . callBPP $
        rideCancel driverToken1 tRide1.id $
          RideAPI.CancelRideReq (SCR.CancellationReasonCode "OTHER") Nothing

      void . poll $
        callBAP (appBookingStatus bBookingId appRegistrationToken)
          <&> (.status)
          >>= (`shouldBe` AppRB.AWAITING_REASSIGNMENT)
          <&> Just

      rideInfo2 <-
        poll . callBPP $
          getNotificationInfo tBooking.id driverToken2
            <&> (.rideRequest)
      rideInfo2.bookingId `shouldBe` tBooking.id

      -- Driver2 Accepts a ride
      void . callBPP $
        rideRespond tBooking.id driverToken2 $
          TbeBookingAPI.SetDriverAcceptanceReq TbeBookingAPI.ACCEPT

      tRide2 <- poll $ do
        tRide <- getBPPRide tBooking.id
        tRide.status `shouldBe` TRide.NEW
        return $ Just tRide

      void . callBPP $
        rideCancel driverToken2 tRide2.id $
          RideAPI.CancelRideReq (SCR.CancellationReasonCode "OTHER") Nothing

      void . poll $
        callBAP (appBookingStatus bBookingId appRegistrationToken)
          <&> (.status)
          >>= (`shouldBe` AppRB.CANCELLED)
          <&> Just

      void . callBPP $ setDriverOnline driverToken1 False
      void . callBPP $ setDriverOnline driverToken2 False
