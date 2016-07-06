{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Referee.User.Routes where

import Servant
import qualified Data.Text as T
import Network.Wai
import Control.Monad.IO.Class

import Referee.Common.Types
import Referee.User.Types
import Referee.User.Api

type UserRoutes =
       "register" :> ReqBody '[JSON] UserRegistration :> Post '[JSON] (Maybe UserId)
  :<|> Get '[JSON] [User]
  :<|> Capture "id" UserId :> Get '[JSON] (Maybe User)
  :<|> "checkname" :> Capture "name" T.Text :> Get '[JSON] Bool

userRoutes :: Proxy UserRoutes
userRoutes = Proxy

userServer :: Interpreter UserF IO -> Server UserRoutes
userServer interpret =
       liftIO . interpret . registerUser
  :<|> liftIO (interpret getUsers)
  :<|> liftIO . interpret . getUser
  :<|> liftIO . interpret . checkName

userApplication :: Interpreter UserF IO -> Application
userApplication interpret = serve userRoutes (userServer interpret)
