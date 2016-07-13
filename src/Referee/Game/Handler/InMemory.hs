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

handleGameF :: TVar (GameMap s) -> Rules c s v -> GameF c s v a -> STM a
handleGameF tvar rules gameF = do
  gameMap <- readTVar tvar
  case gameF of
    Tick gameId time cont -> do
      let (state, gameMap') = handleTick gameId time rules gameMap
      writeTVar tvar gameMap'
      return (cont state)
    AddCommand gameId userId command cont -> do
      let (success, gameMap') = handleAddCommand gameId userId command rules gameMap
      writeTVar tvar gameMap'
      return (cont success)
    Outcome gameId cont -> return (cont (handleOutcome gameId rules gameMap))
    Create matchmaking cont -> do
      let (mgameId, gameMap') = handleCreate matchmaking rules gameMap
      writeTVar tvar gameMap'
      return (cont mgameId)
    View gameId userId cont -> return (cont (handleView gameId userId rules gameMap))

handleTick :: GameId -> Time -> Rules c state v -> GameMap state -> (Maybe state, GameMap state)
handleTick gameId time rules gameMap = case UuidMap.lookup gameId gameMap of
  Nothing -> (Nothing, gameMap)
  Just game -> let state' = rulesTick rules time (gameState game)
                   -- I really want lens here
                   gameMap' = adjust (\_ -> game {gameState = state'}) gameId gameMap
               in  (Just state', gameMap')

handleAddCommand :: GameId -> UserId -> command -> Rules command s v -> GameMap s -> (Bool, GameMap s)
handleAddCommand gameId userId command rules gameMap = case UuidMap.lookup gameId gameMap of
  Nothing -> (False, gameMap)
  Just game -> let (success, state') = rulesCommand rules userId command (gameState game)
                   gameMap' = adjust (\_ -> game {gameState = state'}) gameId gameMap
               in (success, gameMap')

-- returns Nothing, indicating an ongoing game, if the gameId isn't in gameMap...
-- that's probably not the best behavior
-- eventually I'll probably have to add some custom error types
handleOutcome :: GameId -> Rules c s v -> GameMap s -> Maybe Outcome
handleOutcome gameId rules gameMap = do
  game <- UuidMap.lookup gameId gameMap
  rulesOutcome rules (gameState game)

handleCreate :: Matchmaking -> Rules c s v -> GameMap s -> (Maybe GameId, GameMap s)
handleCreate matchmaking rules gameMap = case rulesCreate rules matchmaking of
  Nothing -> (Nothing, gameMap)
  Just game ->
    let (gameId, gameMap') = UuidMap.insert game gameMap
    in (Just gameId, gameMap')

handleView :: GameId -> UserId -> Rules c s view -> GameMap s -> Maybe view
handleView gameId userId rules gameMap = case UuidMap.lookup gameId gameMap of
  Nothing -> Nothing
  Just game -> rulesView rules (gameState game) userId

inMemoryGameHandler :: TVar (GameMap s) -> Rules c s v -> Free (GameF c s v) a -> STM a
inMemoryGameHandler tvar rules = foldFree (handleGameF tvar rules)
