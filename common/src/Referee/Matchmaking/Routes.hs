{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Referee.Matchmaking.Routes where

import Servant

import Referee.Game.Types
import Referee.Matchmaking.Types
import qualified Referee.Authentication as Auth

type MatchmakingRoutes =
       Auth.Auth :> "join-random" :> Post '[JSON] MatchmakingId
  :<|> Auth.Auth :> "create-public" :> Post '[JSON] MatchmakingId
  :<|> Auth.Auth :> "create-private" :> Post '[JSON] MatchmakingId
  :<|> Auth.Auth :> "join" :> Capture "id" MatchmakingId :> Post '[JSON] Bool
  :<|> Auth.Auth :> "start" :> Capture "id" MatchmakingId :> Post '[JSON] (Maybe GameId)

matchmakingRoutes :: Proxy MatchmakingRoutes
matchmakingRoutes = Proxy
