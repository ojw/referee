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

type LoginId = UUID.UUID

-- username is the field we need on the login route to lookup the login record
-- so it would be nice for it to live on the login record

-- I think this is okay...
-- we can have the username be in the jwt used for auth
-- and thus not have to look it up when doing user-things

-- this is a lesson in the drawbacks to low-level, fine-granularity,
-- free-monad based APIs

data Login = Login
  { username :: T.Text
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
