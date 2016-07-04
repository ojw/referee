{-# LANGUAGE TemplateHaskell #-}

module Referee.Config where

import System.Envy
import Web.JWT
import Control.Lens
import Control.Exception (try, SomeException)
import qualified Configuration.Dotenv as Dotenv

data Config = Config
  { _jwtSecret :: Secret }
  deriving Show

makeLenses ''Config

instance FromEnv Config where
  fromEnv = Config <$> fmap secret (env "JWT_SECRET")

-- string as the error type?  In 2016???
getConfig :: IO (Either String Config)
getConfig = decodeEnv

-- can't feel good about this
seedEnv :: IO (Either SomeException ())
seedEnv = try (Dotenv.loadFile False ".env")
