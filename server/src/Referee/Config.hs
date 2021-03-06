{-# LANGUAGE TemplateHaskell #-}

module Referee.Config where

import System.Envy
import Control.Lens
import Data.ByteString as B
import Data.ByteString.Char8 as C
import Control.Exception (try, SomeException)
import qualified Configuration.Dotenv as Dotenv

data Config = Config
  { _jwtSecret :: B.ByteString
  , _bcryptCost :: Int
  , _staticDir :: FilePath
  }
  deriving Show

makeLenses ''Config

instance FromEnv Config where
  fromEnv = Config <$> (envMaybe "JWT_SECRET" .!= C.pack "secret")
                   <*> (envMaybe "BCRYPT_COST" .!= 10)
                   <*> (envMaybe "STATIC_DIR" .!= "./")

-- string as the error type?  In 2016???
getConfig :: IO (Either String Config)
getConfig = decodeEnv

-- can't feel good about this
seedEnv :: IO (Either SomeException ())
seedEnv = try (Dotenv.loadFile False ".env")
