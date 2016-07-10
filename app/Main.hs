module Main where

import Network.Wai.Handler.Warp
import Servant.JS
import qualified Data.ByteString.Char8 as B

import Referee
import qualified Referee.Config as Config

main :: IO ()
main = do
  -- load the environment from a .env file, if one exists
  Config.seedEnv
  -- read configuration from environment variables
  eConfig <- Config.getConfig
  case eConfig of
    Left complaint -> do
      putStrLn "Couldn't generate required config from the environment."
      putStrLn "Here's the complaint the config library threw:"
      putStrLn complaint
    Right config -> do
      -- generate javascript for calling the api
      writeJSForAPI allRoutes vanillaJS "api.js"
      -- initialize the various modules
      userMap <- newUserMap
      mmMap <- newMatchmakingMap
      loginMap <- newLoginMap
      let userHandler = inMemoryUserHandler userMap
          matchmakingHandler = inMemoryMatchmakingHandler mmMap
          loginHandler = inMemoryLoginHandler loginMap
          allApp = allApplication userHandler matchmakingHandler loginHandler (Config._jwtSecret config) (Config._bcryptCost config)
      -- run the app
      run 8081 allApp
