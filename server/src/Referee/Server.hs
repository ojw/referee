{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Referee.Server

  ( AllRoutes
  , allRoutes
  , module Referee.User.Routes
  , module Referee.Matchmaking.Routes
  , allApplication
  )

where

import Servant
import Network.Wai

import Referee.UuidMap

import Referee.User.Routes
import Referee.User.Api
import Referee.User.Server
import Referee.Matchmaking.Routes
import Referee.Matchmaking.Api
import Referee.Matchmaking.Server
import Referee.Login.Routes
import Referee.Login.Api
import Referee.Common.Types
import qualified Referee.Authentication as Auth
import Referee.Game.Api
import Referee.Game.Routes
import Referee.Game.Types
import Referee.Login.Server

import Referee.Routes

allServer
  :: (Monad m1, Monad m2, Translates m1 IO, Translates m2 IO)
  => Interpreter UserF m1
  -> Interpreter MatchmakingF m2
  -> Interpreter LoginF m1
  -> Interpreter (GameF c s v) m2
  -> Secret -- jwt secret hash
  -> Int -- bcrypt cost
  -> FilePath -- static assets
  -> Server AllRoutes
allServer userI matchmakingI loginI gameI secret cost staticDir =
       userServer userI loginI cost
  :<|> matchmakingServer secret matchmakingI gameI
  :<|> loginServer loginI secret
  :<|> serveDirectory staticDir -- wow this should be configured somewhere

allApplication
  :: (Monad m1, Monad m2, Translates m1 IO, Translates m2 IO)
  => Interpreter UserF m1
  -> Interpreter MatchmakingF m2
  -> Interpreter LoginF m1
  -> Interpreter (GameF c s v) m2
  -> Secret -- jwt secret hash
  -> Int -- bcrypt cost
  -> FilePath -- static assets
  -> Application
allApplication userI matchmakingI loginI gameI secret cost staticDir =
  serve allRoutes (allServer userI matchmakingI loginI gameI secret cost staticDir)
