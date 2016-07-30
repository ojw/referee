{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Referee.Login.Routes where

import Servant
import qualified Data.Text as T
import Jose.Jwt

import Referee.Common.Types
import Referee.Login.Types

-- will return a servant error on auth failure
type LoginRoutes =
     Header "email" T.Text
  :> Header "password" T.Text
  :> "login"
  :> Get '[JSON] Jwt

loginRoutes :: Proxy LoginRoutes
loginRoutes = Proxy
