{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Referee.User.Widgets where

import Reflex
import Reflex.Dom
import Servant.Reflex
import Servant.API
import Data.Proxy
import qualified Data.Text as T
import Data.Maybe (fromJust, isJust)
import Control.Error

import Referee.User.Types
import Referee.User.Routes

data PasswordError = InputsDontMatch | FailsPolicy String deriving Show

-- | passwordWidget sticks two password inputs into the DOM
-- and has a value that's Right password if they match and pass the policy function.
-- policy should return Nothing if the password is valid,
-- or Just errorMessage otherwise.
-- Should take some kind of options object for e.g. classes on the inputs.
passwordWidget :: MonadWidget t m => (String -> Maybe String) -> m (Dynamic t (Either PasswordError T.Text))
passwordWidget policy = do
  text "Password"
  passwordInput <- textInput (def { _textInputConfig_inputType = "password"})
  text "Confirm Password"
  passwordConfirm <- textInput (def { _textInputConfig_inputType = "password"})

  let inputVal = _textInput_value passwordInput
      confirmVal = _textInput_value passwordConfirm

  inputsMatch <- combineDyn (==) inputVal confirmVal

  passwordResult <- combineDyn (\match input ->
    if | not match -> Left InputsDontMatch
       | isJust (policy input) -> Left (FailsPolicy (fromJust (policy input)))
       | otherwise -> Right (T.pack input)) inputsMatch inputVal

  return passwordResult

demoPWPolicy :: String -> Maybe String
demoPWPolicy inputPassword = if length inputPassword < 8 then Just "Gotta be 8 characters long" else Nothing

demoNamePolicy :: String -> Maybe String
demoNamePolicy inputName = if length inputName < 4 then Just "Gotta be 4 characters long" else Nothing

-- Not gonna attempt proper validation, but checking for an "@" couldn't hurt.
demoEmailPolicy :: String -> Maybe String
demoEmailPolicy inputEmail = Nothing

data NameError = NameTaken | FailsNamePolicy String deriving Show

nameWidget :: MonadWidget t m => (String -> Maybe String) -> m (Dynamic t (Either NameError T.Text))
nameWidget policy = do
  text "User Name"
  nameInput <- textInput def
  let nameVal = _textInput_value nameInput
  nameResult <- mapDyn (\input ->
    if | isJust (policy input) -> Left (FailsNamePolicy (fromJust (policy input)))
       | otherwise -> Right (T.pack input)) nameVal
  return nameResult

data EmailError = EmailTaken | FailsEmailPolicy String deriving Show

emailWidget :: MonadWidget t m => (String -> Maybe String) -> m (Dynamic t (Either EmailError T.Text))
emailWidget policy = do
  text "Email"
  emailInput <- textInput def
  let emailVal = _textInput_value emailInput
  emailResult <- mapDyn (\input ->
    if | isJust (policy input) -> Left (FailsEmailPolicy (fromJust (policy input)))
       | otherwise -> Right (T.pack input)) emailVal
  return emailResult

registerWidget :: (MonadWidget t m, MonadHold t m) => m ()
registerWidget = do
  let (register :<|> getUsers :<|> getUser :<|> checkName) =
        client
        userRoutes
        Proxy
        (constDyn (BaseFullUrl Http "localhost" 8080 ""))

  divClass "registration" $ do
    nameResult <- nameWidget demoNamePolicy
    emailResult <- emailWidget demoEmailPolicy
    passwordResult <- passwordWidget demoPWPolicy

    -- woops, gotta hush these eithers because they all have different error types
    reg <- $(qDyn [| UserRegistration <$>
                    hush $(unqDyn [|nameResult|]) <*>
                    hush $(unqDyn [|emailResult|]) <*>
                    hush $(unqDyn [|passwordResult|])|])

    reg' <- mapDyn (Control.Error.note "foo") reg

    registerButton <- button "register"

    regResult <- register (current reg') registerButton

    reqShow <- mapDyn show reg'
    dynText reqShow

    pwShow <- mapDyn show passwordResult
    dynText pwShow
