{-# LANGUAGE TemplateHaskell #-}

module Referee.Matchmaking.Types where

import qualified Data.Set as Set
import Control.Lens

type Player = Int

data MatchmakingType = Public | Private | Random -- random needs a better name?

-- okay let's asume matches require an exact number of players?
data Matchmaking = Matchmaking
  { _requiredPlayers :: Int
  , _joinedPlayers   :: Set.Set Player
  , _matchmakingType :: MatchmakingType
  }

makeLenses ''Matchmaking

spaceRemaining :: Matchmaking -> Int
spaceRemaining matchmaking = _requiredPlayers matchmaking - Set.size (_joinedPlayers matchmaking)

matchReady :: Matchmaking -> Bool
matchReady matchmaking = spaceRemaining matchmaking == 0

-- need something more sophisticated for e.g. games with multiple teams, w/ multiple players per team
-- need controls over whether players pick their team, have them randomly assigned, etc
-- so joinedPlayers could be Map Player team, with matchmaking parameterized by a team type
-- ugh so many possible features to consider, what an unpleasant mess

-- let's start w/ what's needed for rps

-- the Nothing case means the match was full
-- this should be an Either later
joinMatch :: Player -> Matchmaking -> Maybe Matchmaking
joinMatch player match = if spaceRemaining match > 0 then Just (over joinedPlayers (Set.insert player) match) else Nothing

-- need a startMatch, but no idea what it should do just yet
newMatchmaking :: MatchmakingType -> Int -> Matchmaking
newMatchmaking mtype count  = Matchmaking count Set.empty mtype

-- k gotta consider a few cases...
-- should support private matchmaking and automatic queues of different types

-- for random / matched queues, the player API should just be
-- joinMatchedQueue queuetype
-- (but this needs to be implemented with both create and join under the covers)

-- for private matches, there's both
-- createPrivateMatchmaking
-- and
-- joinPrivateMatchmaking
