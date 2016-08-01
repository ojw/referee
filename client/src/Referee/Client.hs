{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Referee.Client where

import Servant.API
import Data.Proxy
import Servant.Reflex
-- import Reflex
import Reflex.Dom

import Referee.User.Widgets

runGUI :: MonadWidget t m => m ()
runGUI = do
  registerWidget
