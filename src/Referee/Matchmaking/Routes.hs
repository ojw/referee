{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Referee.Matchmaking.Routes where

import Data.Proxy (Proxy(..))
import Data.UUID (UUID)
import Servant
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad.IO.Class
import qualified Data.Text as T
import Network.Wai

import Referee.Matchmaking.Types
import Referee.Matchmaking.Api
import Referee.Common.Types

type MatchmakingRoutes = Auth :>
      ("join-random" :> Post '[JSON] UUID
  :<|> "create-public" :> Post '[JSON] UUID
  :<|> "create-private" :> Post '[JSON] UUID
  :<|> "join" :> Capture "id" UUID :> Post '[JSON] Bool)

matchmakingRoutes :: Proxy MatchmakingRoutes
matchmakingRoutes = Proxy

matchmakingServer
  :: Translates m IO
  => Interpreter MatchmakingF m
  -> Server MatchmakingRoutes
matchmakingServer interpret mText =
       (liftIO . translate . interpret) (Referee.Matchmaking.Api.joinRandom 1)
  :<|> (liftIO . translate . interpret) (createMatchmaking Public)
  :<|> (liftIO . translate . interpret) (createMatchmaking Private)
  :<|> liftIO . translate . interpret . tryJoin 1

matchmakingApplication :: Interpreter MatchmakingF IO -> Application
matchmakingApplication interpret = serve matchmakingRoutes (matchmakingServer interpret)
