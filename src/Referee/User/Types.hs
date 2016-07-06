{-# LANGUAGE TemplateHaskell #-}

module Referee.User.Types where

import qualified Data.Text as T
import Data.UUID (UUID)
import Data.Aeson.TH
import Data.Aeson

import qualified Referee.Utils as Utils

type Connection = ()

data UserRegistration = UserRegistration
  { registrationName :: T.Text
  , registrationEmail :: T.Text
  }

deriveJSON (defaultOptions {fieldLabelModifier = Utils.labelModifier}) ''UserRegistration

type UserId = UUID

data User = User
  { userName :: T.Text
  , userEmail :: T.Text
  } deriving (Show, Eq)

deriveJSON (defaultOptions {fieldLabelModifier = Utils.labelModifier}) ''User
