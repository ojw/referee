module Referee.Examples.RockPaperScissors where

import Data.Maybe (isJust)

-- import Referee.Game

data Throw = Rock | Paper | Scissors
  deriving Eq

data RPSState= RPSState
  { player1 :: Maybe Throw
  , player2 :: Maybe Throw
  }

data RPSPlayer = Player1 | Player2

type RPSCommand = (RPSPlayer, Throw)

type RPSTime = Double

data RPSOutcome = Player1Wins | Player2Wins | Draw

tick :: RPSTime -> RPSState -> RPSState
tick duration state = state

command :: RPSCommand -> RPSState -> RPSState
command (Player1, throw) state = if isJust (player1 state) then state { player1 = Just throw } else state
command (Player2, throw) state = if isJust (player2 state) then state { player2 = Just throw } else state

outcome' :: Throw -> Throw -> RPSOutcome
outcome' Rock Paper = Player2Wins
outcome' Paper Scissors = Player2Wins
outcome' Scissors Rock = Player2Wins
outcome' t1 t2 = if t1 == t2 then Draw else outcome' t2 t1

outcome :: RPSState -> Maybe RPSOutcome
outcome (RPSState (Just t1) (Just t2)) = Just (outcome' t1 t2)
