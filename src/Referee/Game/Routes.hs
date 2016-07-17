{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Referee.Game.Routes where

import Servant
import Network.Wai
import Control.Monad.IO.Class
import Data.Aeson

import Referee.Common.Types
import Referee.Authentication as Auth
import Referee.Game.Types
import Referee.Game.Api

type GameRoutes command state view =
       Auth.Auth :> "sendcommand" :> Capture "game" GameId :> ReqBody '[JSON] command :> Post '[JSON] Bool
  :<|> Auth.Auth :> "getstate" :> Capture "game" GameId :> Get '[JSON] (Maybe view)

gameRoutes :: Proxy (GameRoutes c s v)
gameRoutes = Proxy

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
