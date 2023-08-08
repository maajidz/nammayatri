module Screens.SubscriptionScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.SubscriptionScreen.Controller (ScreenOutput(..))
import Screens.SubscriptionScreen.View as SubscriptionScreen
import Types.App (FlowBT, GlobalState(..), SUBSCRIPTION_SCREEN_OUTPUT(..), ScreenType(..), NAVIGATION_ACTIONS(..))
import Types.ModifyScreenState (modifyScreenState)


subscriptionScreen :: FlowBT String SUBSCRIPTION_SCREEN_OUTPUT
subscriptionScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ SubscriptionScreen.screen state.subscriptionScreen
  case act of
    HomeScreen updatedState -> do 
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ NAV HomeScreenNav)
    RideHistory updatedState -> do 
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ NAV RideHistoryNav)
    Contest updatedState -> do 
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ NAV ContestNav)
    Alerts updatedState -> do 
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ NAV AlertsNav)
    JoinPlanExit updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ MAKE_PAYMENT updatedState)
    PaymentHistory updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GOTO_PAYMENT_HISTORY updatedState)
    PauseAutoPay updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ PAUSE_AUTOPAY updatedState)
    CancelAutoPayPlan updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ CANCEL_AUTOPAY updatedState)
    SwitchCurrentPlan updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ SWITCH_PLAN updatedState)
    ResumeAutoPayPlan updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ RESUME_AUTOPAY updatedState)