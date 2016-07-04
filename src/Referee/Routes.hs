{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Referee.Routes

  ( AllRoutes
  , allRoutes
  , module Referee.User.Routes
  , module Referee.Matchmaking.Routes
  )

where

import Servant

import Referee.UuidMap

import Referee.User.Routes
import Referee.Matchmaking.Routes

type AllRoutes =
       "user" :> UserRoutes
  :<|> "matchmaking" :> MatchmakingRoutes

allRoutes :: Proxy AllRoutes
allRoutes = Proxy
