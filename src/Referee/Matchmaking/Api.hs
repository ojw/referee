{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Referee.Matchmaking.Api where

import Data.Proxy (Proxy(..))
import Data.UUID (UUID)
import Servant
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad.IO.Class

import Referee.UuidMap
import Referee.Matchmaking.Types
import Referee.Common.Types
import Referee.Config (Config(..))

type MatchmakingApi = Auth :>
      ("join-random" :> Post '[JSON] UUID
  :<|> "create-public" :> Post '[JSON] UUID
  :<|> "create-private" :> Post '[JSON] UUID
  :<|> "join" :> Capture "id" UUID :> Post '[JSON] ()) -- this should indicate success

matchmakingApi :: Proxy MatchmakingApi
matchmakingApi = Proxy

-- hmm... does the server hold a bunch of TVars for different systems?
-- for the in-memory ones, anyhow
-- that seems like it may be reasonable / composable.
matchmakingServer :: TVar MatchmakingServer -> Server MatchmakingApi
matchmakingServer serverVar mJwt =
       (liftIO . atomically) (do
         server <- readTVar serverVar
         let (uuid, server') = joinRandom 1 server
         writeTVar serverVar server'
         return uuid)
  :<|> (liftIO . atomically) (do
         server <- readTVar serverVar
         let (uuid, server') = createPublic 1 server
         writeTVar serverVar server'
         return uuid)
  :<|> (liftIO . atomically) (do
         server <- readTVar serverVar
         let (uuid, server') = createPrivate 1 server
         writeTVar serverVar server'
         return uuid)
  :<|> \uuid -> (liftIO . atomically) (modifyTVar serverVar (joinByUUID 1 uuid))
