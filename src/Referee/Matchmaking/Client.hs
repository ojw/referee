module Referee.Matchmaking.Client where

import Servant
import Servant.Client
import Network.HTTP.Client
import Control.Monad.Trans.Except
import qualified Data.Text as T

import Referee.Common.Types
import Referee.Matchmaking.Routes
import Referee.Matchmaking.Types

-- friggin gotta be a better way to do this...

-- One option would be to include Auth on each route instead of
-- wrapping Auth around all the routes.

-- But I'd rather keep the matchmaking routes as they are, and work around this mess.

-- On the other hand, this might be reasonable -
-- It kinda requires inputing one's auth to get the relevent routes.

-- In fact, I should have auth be a pre-auth-token type
-- and wrap it in 'matchmakingClients'.

matchmakingClients auth = (joinRandom, createPublic, createPrivate, join)
  where
    joinRandom :<|> createPublic :<|> createPrivate :<|> join = client matchmakingRoutes auth
