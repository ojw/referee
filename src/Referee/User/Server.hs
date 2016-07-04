{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Referee.User.Server where

import Servant
import Data.UUID (UUID)
import qualified Data.Text as T
import Control.Concurrent.STM
import Control.Monad.Free

import Control.Lens
import Control.Monad.IO.Class

import Referee.User.Types
import Referee.User.Api

type UserApi =
       "register" :> ReqBody '[JSON] UserRegistration :> Post '[JSON] (Maybe UserId)
  :<|> Get '[JSON] [User]
  :<|> Capture "id" UUID :> Get '[JSON] (Maybe User)
  :<|> "checkname" :> Capture "name" T.Text :> Get '[JSON] Bool

userServer :: UserInterpreter -> Server UserApi
userServer int =
       (\reg -> liftIO (int (registerUser reg)))
  :<|> liftIO (int getUsers)
  :<|> (\userId -> liftIO (int (getUser userId)))
  :<|> (\name -> liftIO (int (checkName name)))
