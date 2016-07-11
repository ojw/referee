{-# LANGUAGE TemplateHaskell #-}

module Referee.Matchmaking.Types where

import qualified Data.Set as Set
import Control.Lens
import Data.UUID (UUID(..))
import Data.Maybe (listToMaybe)

import Referee.User.Types
import Referee.Common.Types
import Referee.User.Types
import Referee.UuidMap as UuidMap

type MatchmakingId = UUID

data MatchmakingError = MatchNotFound | MatchFull

data MatchmakingType = Public | Private | Random -- random needs a better name?
  deriving (Eq, Show)
-- okay let's asume matches require an exact number of players?
data Matchmaking = Matchmaking
  { _requiredUserIds :: Int
  , _joinedUserIds   :: Set.Set UserId
  , _matchmakingType :: MatchmakingType
  , _matchmakingId   :: MatchmakingId
  } deriving Show

-- this probably needs to include some sort of config
-- for creating new Matchmaking records

type MatchmakingServer = UuidMap Matchmaking

makeLenses ''Matchmaking

spaceRemaining :: Matchmaking -> Int
spaceRemaining matchmaking = _requiredUserIds matchmaking - Set.size (_joinedUserIds matchmaking)

isJoinable :: Matchmaking -> Bool
isJoinable matchmaking = spaceRemaining matchmaking > 0

matchReady :: Matchmaking -> Bool
matchReady matchmaking = spaceRemaining matchmaking == 0

-- need something more sophisticated for e.g. games with multiple teams, w/ multiple players per team
-- need controls over whether players pick their team, have them randomly assigned, etc
-- so joinedUserIds could be Map UserId team, with matchmaking parameterized by a team type
-- ugh so many possible features to consider, what an unpleasant mess

-- let's start w/ what's needed for rps

-- This is a low-level join
joinMatch :: UserId -> Matchmaking -> Matchmaking
joinMatch player match = if isJoinable match then (over joinedUserIds (Set.insert player)) match else match

-- need a startMatch, but no idea what it should do just yet
newMatchmaking :: MatchmakingType -> Int -> Matchmaking
newMatchmaking mtype count  = Matchmaking count Set.empty mtype UuidMap.nilId
