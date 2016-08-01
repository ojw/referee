{-# LANGUAGE MultiWayIf #-}

module Referee.User.Widgets where

import Reflex
import Reflex.Dom
import Servant.Reflex
import Servant.API
import Data.Proxy
import Data.Maybe (fromJust, isJust)

import Referee.User.Routes

data PasswordError = InputsDontMatch | FailsPolicy String
  deriving Show

data PasswordResult = ValidPW PlaintextPassword | InvalidPW PasswordError
  deriving Show

type PlaintextPassword = String

-- | passwordWidget sticks two password inputs into the DOM
-- and has a value that's Just password if they match and pass the policy function.
-- policy should return Nothing if the password is valid,
-- or Just errorMessage otherwise.
passwordWidget :: MonadWidget t m => (String -> Maybe String) -> m (Dynamic t PasswordResult)
passwordWidget policy = do
  passwordInput <- textInput (def { _textInputConfig_inputType = "password"})
  passwordConfirm <- textInput (def { _textInputConfig_inputType = "password"})

  let inputVal = _textInput_value passwordInput
      confirmVal = _textInput_value passwordConfirm

  inputsMatch <- combineDyn (==) inputVal confirmVal
  policyResult <- mapDyn policy inputVal
  -- validInput <- combineDyn (\match input -> if match  && policy input then Just input else Nothing) inputsMatch inputVal
  passwordResult <- combineDyn (\match input ->
    if | not match -> InvalidPW InputsDontMatch
       | isJust (policy input) -> InvalidPW (FailsPolicy (fromJust (policy input)))
       | otherwise -> ValidPW input) inputsMatch inputVal

  return passwordResult

registerWidget :: MonadWidget t m => m ()
registerWidget = do
  let (register :<|> getUsers :<|> getUser :<|> checkName) =
        client
        userRoutes
        Proxy
        (constDyn (BaseFullUrl Http "localhost" 8080 ""))

  regNameInput <- textInput def
  regEmailInput <- textInput def
  regPassword <- passwordWidget (const Nothing)

  registerButton <- button "register"

  users <- getUsers registerButton

  pwShow <- mapDyn show regPassword
  dynText pwShow

  return ()
