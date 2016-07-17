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
import Referee.Game.Types
import Referee.Game.Api
import Referee.Matchmaking.Types
import Referee.Matchmaking.Api
import Referee.Common.Types
import qualified Referee.Authentication as Auth

type MatchmakingRoutes =
       Auth.Auth :> "join-random" :> Post '[JSON] MatchmakingId
  :<|> Auth.Auth :> "create-public" :> Post '[JSON] MatchmakingId
  :<|> Auth.Auth :> "create-private" :> Post '[JSON] MatchmakingId
  :<|> Auth.Auth :> "join" :> Capture "id" UUID.UUID :> Post '[JSON] Bool
  :<|> Auth.Auth :> "start" :> Capture "id" MatchmakingId :> Post '[JSON] (Maybe GameId)

matchmakingRoutes :: Proxy MatchmakingRoutes
matchmakingRoutes = Proxy

matchmakingServer
  :: (Monad m, Translates m IO)
  => Secret
  -> Interpreter MatchmakingF m
  -> Interpreter (GameF c s v) m
  -> Server MatchmakingRoutes
matchmakingServer secret interpretMM interpretGame =
       Auth.withAuth secret (\player -> (liftIO . translate . interpretMM) (Referee.Matchmaking.Api.joinRandom player))
  :<|> Auth.withAuth secret (\player -> (liftIO . translate . interpretMM) (createMatchmaking Public))
  :<|> Auth.withAuth secret (\player -> (liftIO . translate . interpretMM) (createMatchmaking Private))
  :<|> Auth.withAuth2 secret (\player -> liftIO . translate . interpretMM . tryJoin player)
  :<|> Auth.withAuth2 secret (\player mmId -> liftIO . translate $ do
         mmm <- interpretMM (getMatchmaking mmId)
         case mmm of
           Nothing -> return Nothing
           Just mm -> do
             maybeGameId <- interpretGame (create mm)
             case maybeGameId of
               Nothing -> return Nothing
               Just gameId -> do
                 interpretMM (closeMatchmaking mmId)
                 return (Just gameId))

matchmakingApplication
  :: (Monad m, Translates m IO)
  => Interpreter MatchmakingF m
  -> Interpreter (GameF c s v) m
  -> Secret
  -> Application
matchmakingApplication interpretMM interpretGame secret = serve matchmakingRoutes  (matchmakingServer secret interpretMM interpretGame)
