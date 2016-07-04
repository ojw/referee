{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Referee.User.Api where

import qualified Data.Text as T
import Control.Monad.Free
import Control.Monad.Free.TH

import Referee.User.Types

data UserF a where
  AddUser :: UserRegistration -> (UserId -> a) -> UserF a
  GetUsers :: ([User] -> a) -> UserF a
  GetUser :: UserId -> (Maybe User -> a) -> UserF a
  CheckName :: T.Text -> (Bool -> a) -> UserF a

deriving instance Functor UserF

type UserInterpreter = forall a . Free UserF a -> IO a

-- type UserApi = Free UserF

makeFree_ ''UserF

-- | `addUser` doesn't check name availability.
-- For that, use `registerUser`.
-- Probably never use addUser.
addUser   :: UserRegistration -> Free UserF UserId
getUsers  :: Free UserF [User]
getUser   :: UserId -> Free UserF (Maybe User)
checkName :: T.Text -> Free UserF Bool

registerUser :: UserRegistration -> Free UserF (Maybe UserId)
registerUser registration = do
  nameAvailable <- checkName (registrationName registration)
  if nameAvailable
    then Just <$> addUser registration
    else return Nothing
