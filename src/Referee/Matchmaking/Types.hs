{-# LANGUAGE TemplateHaskell #-}

module Referee.Matchmaking.Types where

import qualified Data.Set as Set
import Control.Lens
import Data.UUID (UUID(..))
import Data.Maybe (listToMaybe)

import Referee.Common.Types
import Referee.UuidMap as UuidMap

type MatchmakingId = UUID

data MatchmakingError = MatchNotFound | MatchFull

data MatchmakingType = Public | Private | Random -- random needs a better name?
  deriving (Eq, Show)
-- okay let's asume matches require an exact number of players?
data Matchmaking = Matchmaking
  { _requiredPlayers :: Int
  , _joinedPlayers   :: Set.Set Player
  , _matchmakingType :: MatchmakingType
  } deriving Show

-- this probably needs to include some sort of config
-- for creating new Matchmaking records

type MatchmakingServer = UuidMap Matchmaking

makeLenses ''Matchmaking

spaceRemaining :: Matchmaking -> Int
spaceRemaining matchmaking = _requiredPlayers matchmaking - Set.size (_joinedPlayers matchmaking)

isJoinable :: Matchmaking -> Bool
isJoinable matchmaking = spaceRemaining matchmaking > 0

matchReady :: Matchmaking -> Bool
matchReady matchmaking = spaceRemaining matchmaking == 0

-- need something more sophisticated for e.g. games with multiple teams, w/ multiple players per team
-- need controls over whether players pick their team, have them randomly assigned, etc
-- so joinedPlayers could be Map Player team, with matchmaking parameterized by a team type
-- ugh so many possible features to consider, what an unpleasant mess

-- let's start w/ what's needed for rps

-- This is a low-level join
joinMatch :: Player -> Matchmaking -> Matchmaking
joinMatch player match = if isJoinable match then (over joinedPlayers (Set.insert player)) match else match

-- need a startMatch, but no idea what it should do just yet
newMatchmaking :: MatchmakingType -> Int -> Matchmaking
newMatchmaking mtype count  = Matchmaking count Set.empty mtype
