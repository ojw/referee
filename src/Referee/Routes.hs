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
import Referee.Common.Types

type AllRoutes =
       "user" :> UserRoutes
  :<|> "matchmaking" :> MatchmakingRoutes

allRoutes :: Proxy AllRoutes
allRoutes = Proxy

allServer
  :: (Translates m1 IO, Translates m2 IO)
  => Interpreter UserF m1
  -> Interpreter MatchmakingF m2
  -> Server AllRoutes
allServer userI matchmakingI =
       userServer userI
  :<|> matchmakingServer matchmakingI

allApplication
  :: (Translates m1 IO, Translates m2 IO)
  => Interpreter UserF m1
  -> Interpreter MatchmakingF m2
  -> Application
allApplication userI matchmakingI = serve allRoutes (allServer userI matchmakingI)
