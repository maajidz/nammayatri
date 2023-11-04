module Screens.OnBoardingFlow.WelcomeScreen.ScreenData where

import Screens.Types (WelcomeScreenState)

initData :: WelcomeScreenState
initData = {
  data : {
    carouselModel : [
      {image : "carousel_1", title : "The fastest Taxi booking\napp is here!", description : "Our speedy booking process means\nyou get a ride quickly and easily."},
      {image : "carousel_2", title : "No more\nsurge pricing!", description : "Experience fair and \nconsistent fares."},
      {image : "carousel_3", title : "Be a part of the \nNovolution!", description : "Revolutionize your commute \nWhere every journey is a step forward!"}
    ]
  }
}