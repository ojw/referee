{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

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

type AllRoutes =
       "user" :> UserRoutes
  :<|> "matchmaking" :> MatchmakingRoutes

allRoutes :: Proxy AllRoutes
allRoutes = Proxy

allServer :: UserInterpreter -> MatchmakingInterpreter -> Server AllRoutes
allServer userI matchmakingI =
       userServer userI
  :<|> matchmakingServer matchmakingI

allApplication :: UserInterpreter -> MatchmakingInterpreter -> Application
allApplication userI matchmakingI = serve allRoutes (allServer userI matchmakingI)
