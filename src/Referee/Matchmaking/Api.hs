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
import Data.Maybe (listToMaybe)

data MatchmakingF a where
  CreateMatchmaking :: MatchmakingType -> (MatchmakingId -> a) -> MatchmakingF a
  GetMatchmaking :: MatchmakingId -> (Maybe Matchmaking -> a) -> MatchmakingF a
  Join :: Player -> MatchmakingId -> (Bool -> a) -> MatchmakingF a
  PublicMatches :: ([MatchmakingId] -> a) -> MatchmakingF a
  RandomMatches :: ([MatchmakingId] -> a) -> MatchmakingF a

deriving instance Functor MatchmakingF

type MatchmakingInterpreter = forall a . Free MatchmakingF a -> IO a

makeFree_ ''MatchmakingF

createMatchmaking :: MatchmakingType -> Free MatchmakingF MatchmakingId
getMatchmaking :: MatchmakingId -> Free MatchmakingF (Maybe Matchmaking)
join :: Player -> MatchmakingId -> Free MatchmakingF Bool
publicMatches :: Free MatchmakingF [MatchmakingId]
randomMatches :: Free MatchmakingF [MatchmakingId]

tryJoin :: Player -> MatchmakingId -> Free MatchmakingF Bool
tryJoin player matchmakingId = do
  mMatchmaking <- getMatchmaking matchmakingId
  case mMatchmaking of
    Nothing -> return False
    Just matchmaking -> join player matchmakingId

-- still wrong, needs to filter for available randoms
-- however... this turns out to be a bit more of a problem than expected
-- since it's possible for another user to join matches while `player`
-- is trying to join.
-- Is it necessary to just call joinRandom again repeatedly?
-- or to just try the join, then create a random match if that fails?
-- It's actually possible to not be quick enough to join one's own
-- newly-created random branch in strange circumstances.
-- I guess it could just keep re-calling joinRandom.
joinRandom :: Player -> Free MatchmakingF MatchmakingId
joinRandom player = do
  randoms <- randomMatches
  case listToMaybe randoms of
    Nothing -> do
      mmId <- createMatchmaking Random
      join player mmId
      return mmId
    Just mmId -> do
      join player mmId
      return mmId
