module Referee.WidgetUtils where

import Reflex.Dom
import Servant.Reflex

showXhrResponse xhrResponseBody = case xhrResponseBody of
          Just (XhrResponseBody_Default t) -> show t
          Just (XhrResponseBody_Text t) -> show t
          Just (XhrResponseBody_Blob blob) -> "uh, it's a blob"
          Just (XhrResponseBody_ArrayBuffer b) -> show b
          Nothing -> "friggin nothing"


parseReq reqResult = case reqResult of
          ResponseSuccess a xhrResponse -> show (_xhrResponse_responseText xhrResponse)
          ResponseFailure s xhrResponse -> "response failure: " ++ s ++ "|" ++ show (_xhrResponse_responseText xhrResponse) ++ "|" ++ show (_xhrResponse_statusText xhrResponse) ++ "|" ++ showXhrResponse (_xhrResponse_response xhrResponse)
          RequestFailure s -> "request failure: " ++ s
