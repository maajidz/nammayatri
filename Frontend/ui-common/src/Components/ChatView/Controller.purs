{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.ChatView.Controller where

import PrestoDOM.Types.DomAttributes (Padding(..))

data Action = SendMessage
            | SendSuggestion String
            | BackPressed
            | TextChanged String
            | EnableSuggestions
            | Call
            | Navigate
            | NoAction
            | OnImageClick String

type Config = 
  { userConfig :: UserConfig
  , messages :: Array ChatComponent
  , messagesSize :: String
  , sendMessageActive :: Boolean
  , vehicleNo :: String
  , suggestionsList :: Array String
  , suggestionDelay :: Int
  , spanParent :: Boolean
  , showTextEdit :: Boolean
  , hint :: String
  , suggestionHeader :: String
  , emptyChatHeader :: String
  , showHeader :: Boolean
  , showStroke :: Boolean
  , languageKey :: String
  , mapsText :: String
  , grey700 :: String
  , blue600 :: String
  , blue900 :: String
  , enableSuggestionClick :: Boolean
  , transparentGrey :: String
  , green200 :: String
  , grey900 :: String
  , grey800 :: String
  , blue800 :: String
  , white900 :: String
  , black800 :: String
  , black700 :: String
  , canSendSuggestion :: Boolean
  , chatBodyPadding :: Padding
  }

type UserConfig =
  { userName :: String
  , appType :: String
  }

type ChatComponent = {
    message :: String 
  , sentBy :: String 
  , timeStamp :: String
  , type :: String
  , delay :: Int
}

config :: Config
config = 
  {
    userConfig : 
        {
          userName : ""
        , appType : ""
        }
    , messages : []
    , messagesSize : ""
    , sendMessageActive : false
    , vehicleNo : ""
    , suggestionsList : []
    , hint : ""
    , suggestionHeader : ""
    , suggestionDelay : 0
    , spanParent : false
    , emptyChatHeader : ""
    , languageKey : ""
    , mapsText : ""
    , grey700 : ""
    , enableSuggestionClick : true
    , blue900 : ""
    , showHeader : true
    , showStroke : true
    , blue600 : ""
    , transparentGrey : ""
    , showTextEdit : true
    , green200 : ""
    , grey800 : ""
    , blue800 : ""
    , grey900 : ""
    , white900 : ""
    , black800 : ""
    , black700 : ""
    , canSendSuggestion : true
    , chatBodyPadding : (Padding 0 0 0 0)
  }

makeChatComponent' :: String -> String -> String -> String -> Int -> ChatComponent
makeChatComponent' message sender timeStamp type' delay =  {
  "message" : message 
, "sentBy" : sender
, "timeStamp" : timeStamp
, "type" : type'
, delay : delay
}

makeChatComponent :: String -> String -> String -> ChatComponent
makeChatComponent message sender timeStamp =  {
  "message" : message
, "sentBy" : sender
, "timeStamp" : timeStamp
, "type" : "Text"
, delay : 0
}
