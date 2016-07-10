{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Referee.Login.Api where

import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString as B
import Control.Monad.Free
import Control.Monad.Free.TH
import Jose.Jwt

import Referee.Common.Types
import Referee.User.Types
import Referee.Login.Types

data LoginF a where
  LookupLogin :: T.Text -> (Maybe Login -> a) -> LoginF a
  -- username, then password... hope I keep that straight :P
  CreateLogin :: T.Text -> T.Text -> UserId -> (Bool -> a) -> LoginF a

deriving instance Functor LoginF

makeFree_ ''LoginF

lookupLogin  :: T.Text -> Free LoginF (Maybe Login)

verifyCredentials :: T.Text -> B.ByteString -> Free LoginF (Maybe Login)
verifyCredentials name password = do
  maybeLogin <- lookupLogin name
  return $ do -- using Maybe's Monad instance, for convenience
    login <- maybeLogin
    if validatePassword password login
      then return login
      else Nothing
