{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Referee.Matchmaking.Api where

import Data.Proxy (Proxy(..))
import Data.UUID (UUID)
import Servant
import Control.Concurrent.STM.TVar

import Referee.Matchmaking.Types

type MatchmakingApi =
       "joinqueue" :> Post '[JSON] ()
  :<|> "create" :> Post '[JSON] ()
  :<|> "joinprivate" :> Capture "id" UUID :> Post '[JSON] ()

matchmakingApi :: Proxy MatchmakingApi
matchmakingApi = Proxy

-- hmm... does the server hold a bunch of TVars for different systems?
-- for the in-memory ones, anyhow
-- that seems like it may be reasonable / composable.
matchmakingServer :: TVar () -> Server MatchmakingApi
matchmakingServer wat =
       return ()
  :<|> return ()
  :<|> \_ -> return ()
