{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Referee.Login.Widgets where

import Reflex
import Reflex.Dom
import Servant.Reflex
import Servant.API -- ???
import Data.Proxy
import qualified Data.Text as T

import Referee.Login.Types
import Referee.Login.Routes

loginWidget :: MonadWidget t m => m ()
loginWidget = return ()
