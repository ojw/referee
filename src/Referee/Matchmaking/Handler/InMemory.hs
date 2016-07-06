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

joinById :: Player -> MatchmakingId -> MatchmakingServer -> MatchmakingServer
joinById player match server = UuidMap.adjust (joinMatch player) match server

createPublic :: Player -> MatchmakingServer -> (UUID, MatchmakingServer)
createPublic player server = insert (Matchmaking 2 (Set.fromList [player]) Public) server

createPrivate :: Player -> MatchmakingServer -> (UUID, MatchmakingServer)
createPrivate player server = insert (Matchmaking 2 (Set.fromList [player]) Private) server

handleMatchmakingF :: TVar MatchmakingMap -> MatchmakingF a -> IO a
handleMatchmakingF tvar matchmakingF = liftIO . atomically $ do
  matchmakingMap <- readTVar tvar
  case matchmakingF of
    CreateMatchmaking mmType cont -> do
      let (uuid, mmMap') = insert (newMatchmaking mmType 2) matchmakingMap
      writeTVar tvar mmMap'
      return (cont uuid)
    GetMatchmaking mmId cont -> do
      let mmm = UuidMap.lookup mmId matchmakingMap
      return (cont mmm)
    Join player mmId cont -> do
      let mmm = UuidMap.lookup mmId matchmakingMap
          (success, mmMap') = case mmm of
            Nothing -> (False, matchmakingMap)
            Just mm -> if isJoinable mm then (True, adjust (joinMatch player) mmId matchmakingMap) else (False, matchmakingMap)
      writeTVar tvar mmMap'
      return (cont success)
    PublicMatches cont -> do
      let mms = UuidMap.toList matchmakingMap
          publics = map fst . filter (\(_, mm) -> _matchmakingType mm == Public) $ mms
      return (cont publics)
    RandomMatches cont -> do
      let mms = UuidMap.toList matchmakingMap
          randoms = map fst . filter (\(_, mm) -> _matchmakingType mm == Random) $ mms
      return (cont randoms)

inMemoryMatchmakingHandler :: TVar MatchmakingMap -> Free MatchmakingF a -> IO a
inMemoryMatchmakingHandler tvar = foldFree (handleMatchmakingF tvar)
