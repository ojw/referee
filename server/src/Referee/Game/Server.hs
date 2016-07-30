{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Referee.Game.Server where

import Servant
import Network.Wai
import Control.Monad.IO.Class
import Data.Aeson

import Referee.Game.Routes
import Referee.Game.Types
import Referee.Common.Types
import Referee.Game.Api
import Referee.Game.Types
import qualified Referee.Authentication as Auth

gameServer
  :: Translates m IO
  => Secret
  -> Interpreter (GameF c s v) m
  -> Server (GameRoutes c s v)
gameServer secret interpretGame =
       Auth.withAuth3 secret (\player gameId command -> (liftIO . translate . interpretGame) (addCommand gameId player command))
  :<|> Auth.withAuth2 secret (\player gameId -> (liftIO . translate . interpretGame) (view gameId player))

gameApplication
  :: (Translates m IO, ToJSON v, FromJSON c)
  => Interpreter (GameF c s v) m
  -> Secret
  -> Application
gameApplication interpreter secret = serve gameRoutes (gameServer secret interpreter)
