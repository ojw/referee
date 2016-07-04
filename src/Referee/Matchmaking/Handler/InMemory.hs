module Referee.Matchmaking.Handler.InMemory where

import Data.UUID (UUID)
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Free
import Control.Monad.Free.TH

import Referee.Common.Types
import Referee.Matchmaking.Types
import Referee.Matchmaking.Api
import Referee.UuidMap as UuidMap

type MatchmakingMap = UuidMap Matchmaking

newMatchmakingMap :: IO (TVar MatchmakingMap)
newMatchmakingMap = do
  newMap <- emptyIO
  newTVarIO newMap

randomMatches :: MatchmakingServer -> [(UUID, Matchmaking)]
randomMatches server = filter isRandom (UuidMap.toList server)
  where isRandom (uuid, mm) = (_matchmakingType mm) == Random

-- would be better to define this stuff in terms of the free monad actions
joinRandom' :: Player -> MatchmakingServer -> (UUID, MatchmakingServer)
joinRandom' player server = case (listToMaybe . map fst) (randomMatches server) of
  Nothing -> insert (Matchmaking 2 (Set.fromList [player]) Random) server
  Just uuid -> (uuid, joinById player uuid server)

joinById :: Player -> MatchmakingId -> MatchmakingServer -> MatchmakingServer
joinById player match server = UuidMap.update (joinMatch player) match server

createPublic :: Player -> MatchmakingServer -> (UUID, MatchmakingServer)
createPublic player server = insert (Matchmaking 2 (Set.fromList [player]) Public) server

createPrivate :: Player -> MatchmakingServer -> (UUID, MatchmakingServer)
createPrivate player server = insert (Matchmaking 2 (Set.fromList [player]) Private) server

handleMatchmakingF :: TVar MatchmakingMap -> MatchmakingF a -> IO a
handleMatchmakingF tvar matchmakingF = liftIO . atomically $ do
  matchmakingMap <- readTVar tvar
  case matchmakingF of
    JoinRandom cont -> do
      let (uuid, mmMap') = joinRandom' 1 matchmakingMap
      writeTVar tvar mmMap'
      return (cont uuid)
    CreateMatchmaking mmType cont -> do
      let (uuid, mmMap') = insert (newMatchmaking mmType 2) matchmakingMap
      writeTVar tvar mmMap'
      return (cont uuid)
    GetMatchmaking mmId cont -> do
      let mmm = UuidMap.lookup mmId matchmakingMap
      return (cont mmm)
    Join player mmId cont -> do
      let mmMap' = update (joinMatch player) mmId matchmakingMap
      writeTVar tvar mmMap'
      return (cont (member mmId matchmakingMap))

inMemoryMatchmakingHandler :: TVar MatchmakingMap -> Free MatchmakingF a -> IO a
inMemoryMatchmakingHandler tvar = foldFree (handleMatchmakingF tvar)
