{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Referee.Login.Widgets where

import Reflex
import Reflex.Dom
import Servant.Reflex
import Servant.API -- ???
import Data.Proxy
import qualified Data.Text as T

import Jose.Jwt

import Referee.Login.Types
import Referee.Login.Routes
import Referee.WidgetUtils

loginWidget
  :: (MonadWidget t m, MonadHold t m)
  => (Behavior t (Either String T.Text)
   -> Behavior t (Either String T.Text)
   -> Event t ()
   -> m (Event t (ReqResult Jwt)))
  -> m ()
loginWidget login = do
  text "email"
  email <- textInput def
  text "password"
  password <- textInput (def { _textInputConfig_inputType = "password" })
  loginButton <- button "log in"

  eEmailVal <- mapDyn (Right . T.pack) (_textInput_value email)
  ePasswordVal <- mapDyn (Right . T.pack) (_textInput_value password)

  loginResult <- login (current eEmailVal) (current ePasswordVal) loginButton

  el "br" (return ())

  r <- holdDyn "not logged in" $ fmap parseReq loginResult
  dynText r

  return ()
