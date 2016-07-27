module Referee.User.Client where

import Servant
import Servant.Client
import Network.HTTP.Client
import Control.Monad.Trans.Except
import qualified Data.Text as T

import Referee.Common.Types
import Referee.User.Routes
import Referee.User.Types

register :<|> getUsers :<|> getUser :<|> checkName = client userRoutes

register :: UserRegistration T.Text -> Manager -> BaseUrl -> ExceptT ServantError IO (Maybe UserId)

getUsers :: Manager -> BaseUrl -> ExceptT ServantError IO [User]

getUser :: UserId -> Manager -> BaseUrl -> ExceptT ServantError IO (Maybe User)

checkName :: T.Text -> Manager -> BaseUrl -> ExceptT ServantError IO Bool
