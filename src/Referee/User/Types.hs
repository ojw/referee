{-# LANGUAGE TemplateHaskell #-}

module Referee.User.Types where

import qualified Data.Text as T
import Data.UUID (toText, fromText, fromASCIIBytes, UUID)
import Data.Aeson.TH
import Data.Aeson
import qualified Data.Char
import Control.Monad (mzero)
import Servant
import Control.Error

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
  } deriving Show

deriveJSON (defaultOptions {fieldLabelModifier = map Data.Char.toLower . dropWhile (not . Data.Char.isUpper)}) ''User

instance ToJSON UUID where
  toJSON uuid = Data.Aeson.String (toText uuid)

instance FromJSON UUID where
  parseJSON (String t) = case fromText t of
    Just uuid -> return uuid
    Nothing -> mzero

instance FromHttpApiData UUID where
  -- parseUrlPiece :: T.Text -> Either T.Text UUID
  parseUrlPiece t = note (T.pack "breh that uuid is invalid") (fromText t)
  parseHeader b = note (T.pack "breh c'mon, wtf?") (fromASCIIBytes b)
  parseQueryParam t = note (T.pack "ugh") (fromText t)
