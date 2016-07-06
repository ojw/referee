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

-- This could always be a good idea later?
class Interprets f g where
  interpret :: forall a . f a -> Free g a

-- Pretty much just for the Translates STM IO instance.
class Translates m1 m2 where
  translate :: m1 a -> m2 a

-- so then one can write

instance Translates STM IO where
  translate = atomically

instance Translates IO IO where
  translate = id

-- although it's likely that I mostly just care about translating into IO

-- and then... we can have an interpreter for internal api into STM
-- and then can write something like
-- foo :: (Translates m IO) => Interpreter UserF m -> Free UserF a -> IO a
-- foo userAction = translate (handle userAction)

-- and can pass an Interpreter that interprets into STM

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
