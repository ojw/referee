{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Referee.User.Routes where

import Servant
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import Network.Wai
import Control.Monad.IO.Class
import Control.Error
import Data.ByteString.Lazy.Char8 as Char8

import Referee.Common.Types
import Referee.User.Types

import Referee.Login.Types

type UserRoutes =
       "register" :> ReqBody '[JSON] (UserRegistration T.Text) :> Post '[JSON] (Maybe UserId)
  :<|> Get '[JSON] [User]
  :<|> Capture "id" UserId :> Get '[JSON] (Maybe User)
  :<|> "checkname" :> Capture "name" T.Text :> Get '[JSON] Bool

userRoutes :: Proxy UserRoutes
userRoutes = Proxy
