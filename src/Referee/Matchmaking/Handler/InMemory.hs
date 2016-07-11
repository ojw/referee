module Referee.Matchmaking.Handler.InMemory where

import Data.UUID (UUID)
import qualified Data.Set as Set
import Data.Maybe (listToMaybe)
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Free
import Control.Monad.Free.TH

import Referee.User.Types
import Referee.Common.Types
import Referee.User.Types
import Referee.Matchmaking.Types
import Referee.Matchmaking.Api
import Referee.UuidMap as UuidMap

type MatchmakingMap = UuidMap Matchmaking

updateMatchmakingId :: MatchmakingId -> Matchmaking -> Matchmaking
updateMatchmakingId uuid matchmaking = matchmaking { _matchmakingId = uuid }


newMatchmakingMap :: IO (TVar MatchmakingMap)
newMatchmakingMap = do
  newMap <- emptyIO updateMatchmakingId
  newTVarIO newMap

joinById :: UserId -> MatchmakingId -> MatchmakingServer -> MatchmakingServer
joinById player match server = UuidMap.adjust (joinMatch player) match server

createPublic :: UserId -> MatchmakingServer -> (UUID, MatchmakingServer)
createPublic player server = insert (Matchmaking 2 (Set.fromList [player]) Public UuidMap.nilId) server

createPrivate :: UserId -> MatchmakingServer -> (UUID, MatchmakingServer)
createPrivate player server = insert (Matchmaking 2 (Set.fromList [player]) Private UuidMap.nilId) server

handleMatchmakingF :: TVar MatchmakingMap -> MatchmakingF a -> STM a
handleMatchmakingF tvar matchmakingF = do
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
    CloseMatchmaking mmId cont -> do
      let success = UuidMap.member mmId matchmakingMap
          mmMap' = update (\_ -> Nothing) mmId matchmakingMap
      writeTVar tvar mmMap'
      return (cont success)

inMemoryMatchmakingHandler :: TVar MatchmakingMap -> Free MatchmakingF a -> STM a
inMemoryMatchmakingHandler tvar = foldFree (handleMatchmakingF tvar)
