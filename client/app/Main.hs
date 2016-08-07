module Main where

import Reflex.Dom

import Referee.Client

main :: IO ()
main = mainWidget (runGUI "localhost" 8081)
