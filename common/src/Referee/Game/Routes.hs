{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Referee.Game.Routes where

import Servant


import Referee.Authentication as Auth
import Referee.Game.Types

type GameRoutes command state view =
       Auth.Auth :> "sendcommand" :> Capture "game" GameId :> ReqBody '[JSON] command :> Post '[JSON] Bool
  :<|> Auth.Auth :> "getstate" :> Capture "game" GameId :> Get '[JSON] (Maybe view)

gameRoutes :: Proxy (GameRoutes c s v)
gameRoutes = Proxy
