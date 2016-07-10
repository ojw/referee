{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Referee.Matchmaking.Routes where

import Servant
import Control.Monad.IO.Class
import Network.Wai (Application, Request, requestHeaders)
import qualified Data.UUID as UUID

import Referee.User.Types (UserId)
import Referee.Matchmaking.Types
import Referee.Matchmaking.Api
import Referee.Common.Types
import qualified Referee.Authentication as Auth

type MatchmakingRoutes = Auth.WithAuthentication
      ("join-random" :> Post '[JSON] UUID.UUID
  :<|> "create-public" :> Post '[JSON] UUID.UUID
  :<|> "create-private" :> Post '[JSON] UUID.UUID
  :<|> "join" :> Capture "id" UUID.UUID :> Post '[JSON] Bool)

matchmakingRoutes :: Proxy MatchmakingRoutes
matchmakingRoutes = Proxy

matchmakingServer
  :: Translates m IO
  => Interpreter MatchmakingF m
  -> Server MatchmakingRoutes
matchmakingServer interpret =
       (\player -> (liftIO . translate . interpret) (Referee.Matchmaking.Api.joinRandom player))
  :<|> (\player -> (liftIO . translate . interpret) (createMatchmaking Public))
  :<|> (\player -> (liftIO . translate . interpret) (createMatchmaking Private))
  :<|> (\player -> liftIO . translate . interpret . tryJoin player)

matchmakingApplication :: Interpreter MatchmakingF IO -> Secret -> Application
matchmakingApplication interpret secret =
  serveWithContext matchmakingRoutes (Auth.getAuthContext secret) (matchmakingServer interpret)
