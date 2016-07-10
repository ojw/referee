{-# LANGUAGE TemplateHaskell #-}

module Referee.Config where

import System.Envy
import Control.Lens
import Data.ByteString as B
import Control.Exception (try, SomeException)
import qualified Configuration.Dotenv as Dotenv

data Config = Config
  { _jwtSecret :: B.ByteString
  , _bcryptCost :: Int
  }
  deriving Show

makeLenses ''Config

instance FromEnv Config where
  fromEnv = Config <$> (env "JWT_SECRET")
                   <*> (env "BCRYPT_COST")

-- string as the error type?  In 2016???
getConfig :: IO (Either String Config)
getConfig = decodeEnv

-- can't feel good about this
seedEnv :: IO (Either SomeException ())
seedEnv = try (Dotenv.loadFile False ".env")
