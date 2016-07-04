{-# LANGUAGE DataKinds #-}

module Referee.Common.Types where

import Servant.API
import Data.Text (Text)

type Auth = Header "auth" Text

type Player = Int
