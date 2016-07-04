{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Referee.User.Server where

import Servant
import Data.UUID (UUID)
import qualified Data.Text as T
import Control.Concurrent.STM
import Control.Monad.Free
import Network.Wai

import Control.Lens
import Control.Monad.IO.Class

import Referee.User.Types
import Referee.User.Api

type UserApi =
       "register" :> ReqBody '[JSON] UserRegistration :> Post '[JSON] (Maybe UserId)
  :<|> Get '[JSON] [User]
  :<|> Capture "id" UUID :> Get '[JSON] (Maybe User)
  :<|> "checkname" :> Capture "name" T.Text :> Get '[JSON] Bool

userApi :: Proxy UserApi
userApi = Proxy

userServer :: UserInterpreter -> Server UserApi
userServer interpret =
       liftIO . interpret . registerUser
  :<|> liftIO (interpret getUsers)
  :<|> liftIO . interpret . getUser
  :<|> liftIO . interpret . checkName

userApplication :: UserInterpreter -> Application
userApplication interpret = serve userApi (userServer interpret)
