module Referee.Server where

import Referee.UuidMap
import Referee.Game
import Referee.Matchmaking
import Referee.Matchmaking.Server

data Server time command state outcome = Server
  { games :: UuidMap state
  , rules :: Game time command state outcome
  , matchmaking :: UuidMap Matchmaking
  }

-- this should probably require a player, too
createMatchmaking :: Server t c s o -> Server t c s o
createMatchmaking server = server { matchmaking = matchmaking' }
  where matchmaking' = undefined
