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
import qualified Data.Maybe as Maybe

import Referee.Common.Types as Types
import Referee.User.Types
import Referee.Login.Types

data LoginF a where
  LookupLogin :: Types.Email -> (Maybe Login -> a) -> LoginF a
  CreateLogin :: Types.Email -> HashedPassword -> UserId -> (Maybe LoginId -> a) -> LoginF a

deriving instance Functor LoginF

makeFree_ ''LoginF

lookupLogin :: Types.Email -> Free LoginF (Maybe Login)
createLogin :: Types.Email -> HashedPassword -> UserId -> Free LoginF (Maybe LoginId)

verifyCredentials :: Types.Email -> B.ByteString -> Free LoginF (Maybe Login)
verifyCredentials email password = do
  maybeLogin <- lookupLogin email
  return $ do -- using Maybe's Monad instance, for convenience
    login <- maybeLogin
    if validatePassword password login
      then return login
      else Nothing

emailAlreadyRegistered :: Types.Email -> Free LoginF Bool
emailAlreadyRegistered email = fmap Maybe.isJust (lookupLogin email)

-- | Actually checks if the email is in use first.
-- Use this, not regular createLogin
carefulCreateLogin :: Types.Email -> HashedPassword -> UserId -> Free LoginF (Maybe LoginId)
carefulCreateLogin email hashedPass userId = do
  alreadyRegistered <- emailAlreadyRegistered email
  if alreadyRegistered
    then return Nothing
    else createLogin email hashedPass userId
