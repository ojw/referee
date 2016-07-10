{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Referee.Game.Routes where

import Servant
import Network.Wai
import Control.Monad.IO.Class

import Referee.Game.Types

-- gotta get some user auth on these routes
-- hurry, Joe, hurry!
type GameRoutes command state view =
       "sendcommand" :> Capture "game" GameId :> ReqBody '[JSON] command :> Post '[JSON] Bool
  :<|> "getstate" :> Capture "game" GameId :> Get '[JSON] view
