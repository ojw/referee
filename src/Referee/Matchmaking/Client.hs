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
  :: AuthenticateReq (AuthProtect "jwt-user-auth")
  -> Manager
  -> BaseUrl
  -> ClientM MatchmakingId

createPublic
  :: AuthenticateReq (AuthProtect "jwt-user-auth")
  -> Manager
  -> BaseUrl
  -> ClientM MatchmakingId

createPrivate
  :: AuthenticateReq (AuthProtect "jwt-user-auth")
  -> Manager
  -> BaseUrl
  -> ClientM MatchmakingId

join
  :: AuthenticateReq (AuthProtect "jwt-user-auth")
  -> MatchmakingId
  -> Manager
  -> BaseUrl
  -> ClientM Bool

start
  :: AuthenticateReq (AuthProtect "jwt-user-auth")
  -> MatchmakingId
  -> Manager
  -> BaseUrl
  -> ClientM (Maybe GameId)

joinRandom :<|> createPublic :<|> createPrivate :<|> join :<|> start = client matchmakingRoutes
