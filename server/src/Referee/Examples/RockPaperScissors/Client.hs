{-# LANGUAGE DataKinds #-}

module Referee.Example.RockPaperScissors.Client where

import Servant
import Servant.Client
import Network.HTTP.Client (Manager)
import Data.Aeson

import Referee.User.Types
import Referee.Game.Types
import Referee.Game.Routes
import Referee.Matchmaking.Types
import Referee.Authentication

import Referee.Examples.RockPaperScissors.Rules

sendCommand
  :: AuthenticateReq (AuthProtect "jwt-user-auth")
  -> GameId
  -> RPSCommand
  -> Manager
  -> BaseUrl
  -> ClientM Bool

getState
  :: AuthenticateReq (AuthProtect "jwt-user-auth")
  -> GameId
  -> Manager
  -> BaseUrl
  -> ClientM RPSState

sendCommand :<|> getState = client gameRoutes
