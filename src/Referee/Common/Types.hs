{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Referee.Common.Types where

import Servant.API
import Data.Text (Text)
import Data.Aeson
import Control.Error
import Control.Monad (mzero)
import Data.UUID
import qualified Data.Text as T
import Control.Monad.Free (Free)
import Control.Concurrent.STM

type Auth = Header "auth" Text

type Player = Int

type Interpreter f m = forall a . Free f a -> m a
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

instance ToHttpApiData UUID where
  toUrlPiece t = toText t
