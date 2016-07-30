{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Referee.Matchmaking.Server where

import Servant
import Network.Wai
import Control.Monad.IO.Class
import Data.Aeson

import Referee.User.Types (UserId)
import Referee.Matchmaking.Routes
import Referee.Matchmaking.Types
import Referee.Common.Types
import Referee.Matchmaking.Api
import Referee.Matchmaking.Types
import Referee.Game.Api

import qualified Referee.Authentication as Auth


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
