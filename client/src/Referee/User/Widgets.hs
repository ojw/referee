module Referee.User.Widgets where

import Reflex
import Reflex.Dom
import Servant.Reflex
import Servant.API
import Data.Proxy

import Referee.User.Routes

-- better would be passwordWidget :: MonadWidget t m => m (Maybe Password)
-- that validates some policy and that the PWs match
-- and the widget has both password and confirmation
passwordWidget :: MonadWidget t m => m (TextInput t)
passwordWidget = textInput (def { _textInputConfig_inputType = "password"})

registerWidget :: MonadWidget t m => m ()
registerWidget = do
  let (register :<|> getUsers :<|> getUser :<|> checkName) =
        client
        userRoutes
        Proxy
        (constDyn (BaseFullUrl Http "localhost" 8080 ""))

  regNameInput <- textInput def
  regEmailInput <- textInput def
  regPasswordInput <- passwordWidget
  regPasswordConfInput <- passwordWidget



  registerButton <- button "register"

  users <- getUsers registerButton

  return ()
