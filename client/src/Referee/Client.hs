{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Referee.Client where

import Servant.API
import Data.Proxy
import Servant.Reflex
-- import Reflex
import Reflex.Dom

runGUI :: MonadWidget t m => m ()
runGUI = do
  text "hello"
  -- let (getUsers :<|> lookupUser :<|> registerUser) =
  --       client
  --       bigTemplateRoutes
  --       Proxy
  --       (constDyn (BaseFullUrl Http "localhost" 8080 ""))

  -- let req = xhrRequest "get" "http://localhost:8080/users" def

  -- testBtn <- button "test"
  -- test :: Event t XhrResponse <- performRequestAsync $ fmap (\_ -> req) testBtn

  -- x <- holdDyn "foo" $ fmap show $ fmap _xhrResponse_responseText test
  -- dynText x

  -- usersBtn <- button "get users"
  -- --users :: Event t (ReqResult [User]) <- getUsers usersBtn
  -- users :: Event t (ReqResult Int) <- getUsers usersBtn



  -- let showXhrResponse xhrResponseBody = case xhrResponseBody of
  --       Just (XhrResponseBody_Default t) -> show t
  --       Just (XhrResponseBody_Text t) -> show t
  --       Just (XhrResponseBody_Blob blob) -> "uh, it's a blob"
  --       Just (XhrResponseBody_ArrayBuffer b) -> show b
  --       Nothing -> "friggin nothing"

  -- let parseReq reqResult = case reqResult of
  --       ResponseSuccess a xhrResponse -> "woo"
  --       ResponseFailure s xhrResponse -> "response failure: " ++ s ++ "|" ++ show (_xhrResponse_responseText xhrResponse) ++ "|" ++ show (_xhrResponse_statusText xhrResponse) ++ "|" ++ showXhrResponse (_xhrResponse_response xhrResponse)
  --       RequestFailure s -> "request failure: " ++ s

  -- r <- holdDyn "Waiting" $ fmap parseReq users
  -- dynText r
  --                                                    -- (constDyn (BasePath "/"))
  -- return ()
