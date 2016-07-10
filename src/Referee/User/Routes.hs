{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Referee.User.Routes where

import Servant
import qualified Data.Text as T
import Network.Wai
import Control.Monad.IO.Class

import Referee.Common.Types
import Referee.User.Types
import Referee.User.Api

import Referee.Login.Types
import Referee.Login.Api

type UserRoutes =
       "register" :> ReqBody '[JSON] (UserRegistration T.Text) :> Post '[JSON] (Maybe UserId)
  :<|> Get '[JSON] [User]
  :<|> Capture "id" UserId :> Get '[JSON] (Maybe User)
  :<|> "checkname" :> Capture "name" T.Text :> Get '[JSON] Bool

userRoutes :: Proxy UserRoutes
userRoutes = Proxy

-- alas, creating a user requires creating a login,
-- so the userServer needs access to an interpreter for login...
userServer
  :: (Translates m1 IO, Translates m2 IO)
  => Interpreter UserF m1
  -> Interpreter LoginF m2
  -> Server UserRoutes
userServer interpretUser interpretLogin =
       liftIO . translate . interpretUser . registerUser
  :<|> (liftIO . translate) (interpretUser getUsers)
  :<|> liftIO . translate . interpretUser . getUser
  :<|> liftIO . translate . interpretUser . checkName

userApplication
  :: (Translates m1 IO, Translates m2 IO)
  => Interpreter UserF m1
  -> Interpreter LoginF m2
  -> Application
userApplication interpretUser interpretLogin = serve userRoutes (userServer interpretUser interpretLogin)
