{-# LANGUAGE DeriveGeneric #-}

module Referee.Examples.RockPaperScissors.Rules where

import Data.Maybe (isNothing)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Aeson
import GHC.Generics

import Control.Lens -- because of typed hole complaints

import Referee.UuidMap
import Referee.Matchmaking.Types
import Referee.Game
import Referee.User.Types (UserId)

data Throw = Rock | Paper | Scissors
  deriving (Eq, Generic)

instance ToJSON Throw
instance FromJSON Throw

data RPSState= RPSState
  { player1 :: (UserId, Maybe Throw)
  , player2 :: (UserId, Maybe Throw)
  } deriving Generic

instance FromJSON RPSState

data RPSPlayer = Player1 | Player2 deriving Generic

instance ToJSON RPSPlayer

type RPSCommand = (RPSPlayer, Throw)

type RPSTime = Double

data RPSOutcome = Player1Wins | Player2Wins | PlayersDraw

tick :: RPSTime -> RPSState -> RPSState
tick duration state = state

rpsCommand :: UserId -> RPSCommand -> RPSState -> (Bool, RPSState)
rpsCommand userId (Player1, throw) state
  | isNothing (snd (player1 state)) && userId == fst (player1 state) =
      (True, state { player1 = (userId, Just throw) })
  | otherwise = (False, state)
rpsCommand userId (Player2, throw) state
  | isNothing (snd (player2 state)) && userId == fst (player2 state) =
      (True, state { player2 = (userId, Just throw) })
  | otherwise = (False, state)

outcome' :: Throw -> Throw -> RPSOutcome
outcome' Rock Paper = Player2Wins
outcome' Paper Scissors = Player2Wins
outcome' Scissors Rock = Player2Wins
outcome' t1 t2 = if t1 == t2 then PlayersDraw else outcome' t2 t1

rpsOutcome :: RPSState -> Maybe Outcome
rpsOutcome (RPSState (player1, Just t1) (player2, Just t2)) = case outcome' t1 t2 of
  Player1Wins -> Just (WinnerIs player1)
  Player2Wins -> Just (WinnerIs player2)
  PlayersDraw -> Just Draw
rpsOutcome state = Nothing

rpsCreate :: Matchmaking -> Maybe (Game RPSState)
rpsCreate mm = case Set.toList (Control.Lens.view joinedUserIds mm) of
  [player1, player2] -> Just (Game
                              (RPSState (player1, Nothing) (player2, Nothing))
                              nilId
                              [player1, player2])
  _ -> Nothing

rpsRules :: Rules RPSCommand RPSState RPSState
rpsRules = Rules
  { rulesTick = \time -> id
  , rulesCommand = rpsCommand
  , rulesOutcome = rpsOutcome
  , rulesCreate = rpsCreate
  , rulesView = \state _ -> Just state
  }
