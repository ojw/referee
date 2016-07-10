module Referee.Game.Types where

import qualified Data.UUID

import Referee.Matchmaking.Types
import Referee.User.Types

-- for now, let's make some assumptions about time and outcomes,
-- and leave 'command' and 'state' to game developers

type Time = Double

-- way too simple; a placeholder
data Outcome = Win | Loss | Draw

data Rules command state view = Rules
  { rulesTick    :: Time -> state -> state
  -- because a user might not be allower to send a certain command
  -- e.g. if the user isn't in the game, or the command is illegal
  , rulesCommand :: UserId -> command -> state -> (Bool, state)
  -- Nothing means the game isn't over
  , rulesOutcome :: state -> Maybe Outcome
  -- Nothing means matchmaking wasn't actually ready to start
  , rulesCreate  :: Matchmaking -> Maybe (Game state)
  -- Nothing means the user isn't entitled to a view of the state
  , rulesView :: state -> UserId -> Maybe view
  }

type GameId = Data.UUID.UUID

data Game state = Game
  { gameState :: state
  , gameId :: GameId
  -- possibly unnecessary?
  -- but it's here to screen out non-users sending commands
  -- probably unnecessary for right now...
  -- a game or matchmaking should be able to determine rules for
  -- who can view a game, or possibly even send commands
  , gamePlayers :: [UserId]
  }
