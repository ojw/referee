module Referee.Login.Client

  ( login )

where

import qualified Data.Text as T
import Servant.Client
import Network.HTTP.Client
import Control.Error
import Jose.Jwt

import Referee.Login.Routes

login' :: Maybe T.Text -> Maybe T.Text -> Manager -> BaseUrl -> ClientM Jwt
login' = client loginRoutes

login :: T.Text -> T.Text -> Manager -> BaseUrl -> ClientM Jwt
login username password manager baseurl = login' (Just username) (Just password) manager baseurl
