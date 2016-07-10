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
import Referee.Matchmaking.Types (MatchmakingId)
import Referee.Game.Types

data GameF command state view a where
  Tick :: GameId -> Time -> Rules command state view -> (state -> a) -> GameF command state view a
  AddCommand :: GameId -> UserId -> command -> Rules command state view -> (Bool -> a) -> GameF command state view a
  Outcome :: GameId -> Rules command state view -> (Maybe Outcome -> a) -> GameF command state view a
  Create :: MatchmakingId -> Rules command state view -> (Maybe GameId -> a) -> GameF command state view a
  View :: GameId -> UserId -> Rules command state view -> (Maybe view -> a) -> GameF command state view a

deriving instance Functor (GameF command state view)

makeFree_ ''GameF

-- _c_ommand _s_tate _v_iew
tick :: GameId -> Time -> Rules c s v -> Free (GameF c s v) s
addCommand :: GameId -> UserId -> c -> Rules c s v -> Free (GameF c s v) Bool
outcome :: GameId -> Rules c s v -> Free (GameF c s v) (Maybe Outcome)
create :: MatchmakingId -> Rules c s v -> Free (GameF c s v) (Maybe GameId)
view :: GameId -> UserId -> Rules c s v -> Free (GameF c s v) (Maybe v)
