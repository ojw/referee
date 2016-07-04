module Referee

  -- This is mostly arbitrary export list, based on the needs of Main.

  ( inMemoryUserHandler
  , inMemoryMatchmakingHandler
  , allApplication
  , allRoutes
  , newUserMap
  , newMatchmakingMap
  )

where

import Referee.Routes
import Referee.User
import Referee.Matchmaking

-- gotta map a game's internal player model w/ the server's player model
