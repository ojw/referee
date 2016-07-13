module Referee.Examples.RockPaperScissors.Server where

import Servant
import Network.Wai
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Control.Monad.Free

import Referee.Common.Types
import Referee.Authentication as Auth
import Referee.Game

import Referee.Examples.RockPaperScissors.Rules

rpsInterpreter :: TVar (GameMap RPSState) -> Free (GameF RPSCommand RPSState RPSState) a -> STM a
rpsInterpreter = inMemoryGameHandler rpsRules

rpsServer :: TVar (GameMap RPSState) -> Server (GameRoutes RPSCommand RPSState RPSState)
rpsServer state = gameServer (rpsInterpreter state)

rpsApplication :: TVar (GameMap RPSState) -> Secret -> Application
rpsApplication state secret = serveWithContext gameRoutes (Auth.getAuthContext secret) (rpsServer state)
