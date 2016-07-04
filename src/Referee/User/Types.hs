{-# LANGUAGE TemplateHaskell #-}

module Referee.User.Types where

import qualified Data.Text as T
import Data.UUID (UUID)
import Data.Aeson.TH
import qualified Data.Char

type Connection = ()

data UserRegistration = UserRegistration
  { registrationName :: T.Text
  , registrationEmail :: T.Text
  }

deriveJSON (defaultOptions {fieldLabelModifier = map Data.Char.toLower . dropWhile (not . Data.Char.isUpper)}) ''UserRegistration

type UserId = UUID

data User = User
  { userName :: T.Text
  , userEmail :: T.Text
  }
