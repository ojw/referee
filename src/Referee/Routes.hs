{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Referee.Routes

  ( AllRoutes
  , allRoutes
  , module Referee.User.Routes
  , module Referee.Matchmaking.Routes
  , allApplication
  )

where

import Servant
import Network.Wai

import Referee.UuidMap

import Referee.User.Routes
import Referee.User.Api
import Referee.Matchmaking.Routes
import Referee.Matchmaking.Api
import Referee.Login.Routes
import Referee.Login.Api
import Referee.Common.Types
import qualified Referee.Authentication as Auth

type AllRoutes =
       "user" :> UserRoutes
  :<|> "matchmaking" :> MatchmakingRoutes
  :<|> "login" :> LoginRoutes

allRoutes :: Proxy AllRoutes
allRoutes = Proxy

allServer
  :: (Monad m1, Translates m1 IO, Translates m2 IO)
  => Interpreter UserF m1
  -> Interpreter MatchmakingF m2
  -> Interpreter LoginF m1
  -> Secret -- jwt secret hash
  -> Int -- bcrypt cost
  -> Server AllRoutes
allServer userI matchmakingI loginI secret cost =
       userServer userI loginI cost
  :<|> matchmakingServer matchmakingI
  :<|> loginServer loginI secret

allApplication
  :: (Monad m1, Translates m1 IO, Translates m2 IO)
  => Interpreter UserF m1
  -> Interpreter MatchmakingF m2
  -> Interpreter LoginF m1
  -> Secret -- jwt secret hash
  -> Int -- bcrypt cost
  -> Application
allApplication userI matchmakingI loginI secret cost =
  serveWithContext allRoutes (Auth.getAuthContext secret) (allServer userI matchmakingI loginI secret cost)
