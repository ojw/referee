{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Referee.Game.Api where

import Control.Monad.Free
import Control.Monad.Free.TH

import Referee.User.Types (UserId)
import Referee.Matchmaking.Types (Matchmaking)
import Referee.Game.Types

data GameF command state view a where
  Tick :: GameId -> Time -> (Maybe state -> a) -> GameF command state view a
  -- should probably use more descriptive errors than False
  AddCommand :: GameId -> UserId -> command -> (Bool -> a) -> GameF command state view a
  Outcome :: GameId -> (Maybe Outcome -> a) -> GameF command state view a
  Create :: Matchmaking -> (Maybe GameId -> a) -> GameF command state view a
  View :: GameId -> UserId -> (Maybe view -> a) -> GameF command state view a

deriving instance Functor (GameF command state view)

makeFree_ ''GameF

-- _c_ommand _s_tate _v_iew
tick :: GameId -> Time -> Free (GameF c s v) (Maybe s)
addCommand :: GameId -> UserId -> c -> Free (GameF c s v) Bool
outcome :: GameId -> Free (GameF c s v) (Maybe Outcome)
create :: Matchmaking -> Free (GameF c s v) (Maybe GameId)
view :: GameId -> UserId -> Free (GameF c s v) (Maybe v)
