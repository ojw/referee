{-# LANGUAGE DataKinds #-}

module Referee.Common.Types where

import Servant.API
import Data.Text (Text)
import Data.Aeson
import Control.Error
import Control.Monad (mzero)
import Data.UUID
import qualified Data.Text as T

type Auth = Header "auth" Text

type Player = Int

instance ToJSON UUID where
  toJSON uuid = Data.Aeson.String (toText uuid)

instance FromJSON UUID where
  parseJSON (String t) = case fromText t of
    Just uuid -> return uuid
    Nothing -> mzero

instance FromHttpApiData UUID where
  parseUrlPiece t = note (T.pack "breh that uuid is invalid") (fromText t)
  parseHeader b = note (T.pack "breh c'mon, wtf?") (fromASCIIBytes b)
  parseQueryParam t = note (T.pack "ugh") (fromText t)
