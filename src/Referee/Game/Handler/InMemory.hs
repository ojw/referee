module Referee.Game.Handler.InMemory where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Free

import Referee.User.Types
import Referee.Matchmaking.Types
import Referee.Game.Types
import Referee.Game.Routes
import Referee.Game.Api

import Referee.UuidMap as UuidMap

updateGameId :: GameId -> Game state -> Game state
updateGameId uuid game = game { gameId = uuid }

type GameMap state = UuidMap (Game state)

newGameMap :: IO (TVar (GameMap state))
newGameMap = do
  newMap <- emptyIO updateGameId
  newTVarIO newMap

handleGameF :: TVar (GameMap s) -> GameF c s v a -> STM a
handleGameF tvar gameF = do
  gameMap <- readTVar tvar
  case gameF of
    Tick gameId time rules cont -> do
      let (state, gameMap') = handleTick gameId time rules gameMap
      writeTVar tvar gameMap'
      return (cont state)
    AddCommand gameId userId command rules cont -> do
      let (success, gameMap') = handleAddCommand gameId userId command rules gameMap
      writeTVar tvar gameMap'
      return (cont success)
    Outcome gameId rules cont -> return (cont (handleOutcome gameId rules gameMap))
    Create matchmaking rules cont -> do
      let (mgameId, gameMap') = handleCreate matchmaking rules gameMap
      writeTVar tvar gameMap'
      return (cont mgameId)
    View gameId userId rules cont -> return (cont (handleView gameId userId rules gameMap))

handleTick :: GameId -> Time -> Rules c state v -> GameMap state -> (state, GameMap state)
handleTick = undefined

handleAddCommand :: GameId -> UserId -> command -> Rules command s v -> GameMap s -> (Bool, GameMap s)
handleAddCommand = undefined

handleOutcome :: GameId -> Rules c s v -> GameMap s -> Maybe Outcome
handleOutcome = undefined

handleCreate :: MatchmakingId -> Rules c s v -> GameMap s -> (Maybe GameId, GameMap s)
handleCreate = undefined

handleView :: GameId -> UserId -> Rules c s view -> GameMap s -> Maybe view
handleView = undefined
