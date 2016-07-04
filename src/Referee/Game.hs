module Referee.Game where

data Game time command state outcome = Game
  { tick    :: time    -> state -> state
  , command :: command -> state -> state
  , outcome :: state -> Maybe outcome
  }
