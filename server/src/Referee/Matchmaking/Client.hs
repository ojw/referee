{-# LANGUAGE DataKinds #-}

module Referee.Matchmaking.Client where

import Servant
import Servant.Client
import Network.HTTP.Client (Manager)

import Referee.User.Types
import Referee.Game.Types
import Referee.Matchmaking.Types
import Referee.Matchmaking.Routes
import Referee.Authentication

joinRandom
  :: ClientAuth
  -> Manager
  -> BaseUrl
  -> ClientM MatchmakingId

createPublic
  :: ClientAuth
  -> Manager
  -> BaseUrl
  -> ClientM MatchmakingId

createPrivate
  :: ClientAuth
  -> Manager
  -> BaseUrl
  -> ClientM MatchmakingId

join
  :: ClientAuth
  -> MatchmakingId
  -> Manager
  -> BaseUrl
  -> ClientM Bool

start
  :: ClientAuth
  -> MatchmakingId
  -> Manager
  -> BaseUrl
  -> ClientM (Maybe GameId)

joinRandom :<|> createPublic :<|> createPrivate :<|> join :<|> start = client matchmakingRoutes
