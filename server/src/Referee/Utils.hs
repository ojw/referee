module Referee.Utils where

import qualified Data.Char

-- | 'labelModifier' is useful when generating names with template haskell.
--
-- It drops characters til it hits the first upper-case letter,
-- then downcases what's left.
--
-- So
--
-- >>> labelModifier "_someName"
-- "name"
--
-- >>> labelModifier "fooBar"
-- "bar"
--
-- ... which is how I like it.
labelModifier :: String -> String
labelModifier = map Data.Char.toLower . dropWhile (not . Data.Char.isUpper)
