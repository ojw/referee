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


type MatchmakingRoutes = Auth.WithAuthentication
      ("join-random" :> Post '[JSON] MatchmakingId
  :<|> "create-public" :> Post '[JSON] MatchmakingId
  :<|> "create-private" :> Post '[JSON] MatchmakingId
  :<|> "join" :> Capture "id" UUID.UUID :> Post '[JSON] Bool
  :<|> "start" :> Capture "id" MatchmakingId :> Post '[JSON] (Maybe GameId))

matchmakingRoutes :: Proxy MatchmakingRoutes
matchmakingRoutes = Proxy

matchmakingServer
  :: (Monad m, Translates m IO)
  => Interpreter MatchmakingF m
  -> Interpreter (GameF c s v) m
  -> Rules c s v
  -> Server MatchmakingRoutes
matchmakingServer interpretMM interpretGame rules =
       (\player -> (liftIO . translate . interpretMM) (Referee.Matchmaking.Api.joinRandom player))
  :<|> (\player -> (liftIO . translate . interpretMM) (createMatchmaking Public))
  :<|> (\player -> (liftIO . translate . interpretMM) (createMatchmaking Private))
  :<|> (\player -> liftIO . translate . interpretMM . tryJoin player)
  :<|> \player mmId -> liftIO . translate $ do
         mmm <- interpretMM (getMatchmaking mmId)
         case mmm of
           Nothing -> return Nothing
           Just mm -> do
             maybeGameId <- interpretGame (create mm)
             case maybeGameId of
               Nothing -> return Nothing
               Just gameId -> do
                 interpretMM (closeMatchmaking mmId)
                 return (Just gameId)

matchmakingApplication
  :: (Monad m, Translates m IO)
  => Interpreter MatchmakingF m
  -> Interpreter (GameF c s v) m
  -> Rules c s v
  -> Secret
  -> Application
matchmakingApplication interpretMM interpretGame rules secret = serveWithContext matchmakingRoutes (Auth.getAuthContext secret)  (matchmakingServer interpretMM interpretGame rules)
