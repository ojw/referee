{-# LANGUAGE TemplateHaskell #-}

module Referee.Matchmaking.Types where

import qualified Data.Set as Set
import Control.Lens
import Data.UUID (UUID(..))
import Data.Maybe (listToMaybe)

import Referee.UuidMap as UuidMap

type Player = Int

data MatchmakingType = Public | Private | Random -- random needs a better name?
  deriving (Eq)
-- okay let's asume matches require an exact number of players?
data Matchmaking = Matchmaking
  { _requiredPlayers :: Int
  , _joinedPlayers   :: Set.Set Player
  , _matchmakingType :: MatchmakingType
  }

                 -- this probably needs to include some sort of config
                 -- for creating new Matchmaking records
type MatchmakingServer = UuidMap Matchmaking

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

randomMatches :: MatchmakingServer -> [(UUID, Matchmaking)]
randomMatches server = filter isRandom (UuidMap.toList server)
  where isRandom (uuid, mm) = (_matchmakingType mm) == Random

        -- omg where does that 2 come from, right?
        -- need a way to have some assumed matchmaking config,
        -- probably as a part of a MatchmakingServer
joinRandom :: Player -> MatchmakingServer -> (UUID, MatchmakingServer)
joinRandom player server = case (listToMaybe . map fst) (randomMatches server) of
  Nothing -> insert (Matchmaking 2 (Set.fromList [player]) Random) server
  Just uuid -> (uuid, joinByUUID player uuid server)

-- this name is bad, but joinMatch is taken
-- gotta rename things soon
joinByUUID :: Player -> UUID -> MatchmakingServer -> MatchmakingServer
joinByUUID player match server = UuidMap.update (joinMatch player) match server

createPublic :: Player -> MatchmakingServer -> (UUID, MatchmakingServer)
createPublic player server = insert (Matchmaking 2 (Set.fromList [player]) Public) server

createPrivate :: Player -> MatchmakingServer -> (UUID, MatchmakingServer)
createPrivate player server = insert (Matchmaking 2 (Set.fromList [player]) Private) server

emptyServerIO :: IO MatchmakingServer
emptyServerIO = UuidMap.emptyIO
