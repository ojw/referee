module Referee.Login.Types

  ( Login (..)
  -- don't want to export the constructor on this guy
  -- so it's impossible to make a HashedPassword that isn't hashed
  , HashedPassword
  , hashPassword
  , validatePassword
  , LoginId
  )

where

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Crypto.KDF.BCrypt as BCrypt
import qualified Data.UUID as UUID

import Referee.User.Types
import qualified Referee.Common.Types as Types

type LoginId = UUID.UUID

data Login = Login
  { email :: Types.Email
  , hashedPassword :: HashedPassword
  , userId :: UserId
  , loginId :: LoginId
  }

newtype HashedPassword = HashedPassword { hashedBytes :: B.ByteString }
  deriving (Eq)

hashPassword :: Int -> B.ByteString -> IO HashedPassword
hashPassword cost password = HashedPassword <$> BCrypt.hashPassword cost password

validatePassword :: B.ByteString -> Login -> Bool
validatePassword password login = BCrypt.validatePassword password (hashedBytes (hashedPassword login))
