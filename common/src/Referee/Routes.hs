{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Referee.Routes where

import Servant

import Referee.User.Routes
import Referee.Login.Routes
import Referee.Game.Routes
import Referee.Matchmaking.Routes

type AllRoutes =
       "user" :> UserRoutes
  :<|> "matchmaking" :> MatchmakingRoutes
  :<|> "login" :> LoginRoutes
  :<|> "client" :> Raw -- for the client assets

allRoutes :: Proxy AllRoutes
allRoutes = Proxy
