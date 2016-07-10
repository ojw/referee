module Referee.Matchmaking.Client where

import Servant
import Servant.Client

import Referee.Matchmaking.Routes

joinRandom :<|> createPublic :<|> createPrivate :<|> join = client matchmakingRoutes
