{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Referee.User.Types where

import GHC.Generics
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.UUID (UUID)
import Data.Aeson.TH
import Data.Aeson

import qualified Referee.Utils as Utils

type Connection = ()

-- | Should only export a function that hashes its input, no normal constructors.
-- So it's impossible to mess up and pass an unhashed password or something.

data UserRegistration password = UserRegistration
  { registrationName :: T.Text
  , registrationEmail :: T.Text
  -- from the web, this'll be T.Text;
  -- internally, this'll be immediately converted to a HashedPassword
  , registrationPassword :: password
  } deriving Generic

-- I should probably write this instance by hand
instance FromJSON (UserRegistration T.Text)

-- required for the generated haskell client code
instance ToJSON (UserRegistration T.Text)

type UserId = UUID

data User = User
  { userName :: T.Text
  , userEmail :: T.Text
  } deriving (Show, Eq)

deriveJSON (defaultOptions {fieldLabelModifier = Utils.labelModifier}) ''User
