{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Referee.Matchmaking.Api where

import Referee.UuidMap
import Referee.Matchmaking.Types
import Referee.Common.Types

import Control.Monad.Free
import Control.Monad.Free.TH

data MatchmakingF a where
  JoinRandom :: (MatchmakingId -> a) -> MatchmakingF a
  CreateMatchmaking :: MatchmakingType -> (MatchmakingId -> a) -> MatchmakingF a
  GetMatchmaking :: MatchmakingId -> (Maybe Matchmaking -> a) -> MatchmakingF a
  Join :: Player -> MatchmakingId -> (Bool -> a) -> MatchmakingF a

deriving instance Functor MatchmakingF

type MatchmakingInterpreter = forall a . Free MatchmakingF a -> IO a

makeFree_ ''MatchmakingF

joinRandom :: Free MatchmakingF MatchmakingId
createMatchmaking :: MatchmakingType -> Free MatchmakingF MatchmakingId
getMatchmaking :: MatchmakingId -> Free MatchmakingF (Maybe Matchmaking)

tryJoin :: Player -> MatchmakingId -> Free MatchmakingF Bool
tryJoin player matchmakingId = do
  mMatchmaking <- getMatchmaking matchmakingId
  case mMatchmaking of
    Nothing -> return False
    Just matchmaking -> join player matchmakingId
