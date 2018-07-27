module LMEmailFinderServer(appSetup) where
import Data.Either

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import EmailFinder as E
import Effect.Aff (Aff)
import Node.Express.App (App, listenHttp, useOnError, get, post, use, setProp)
import Node.Express.Handler (Handler, HandlerM(..), next, nextThrow)
import Node.Express.Request (getRouteParam, getQueryParam, getOriginalUrl, setUserData, getUserData, getBodyParam)
import Node.Express.Response (sendJson, setStatus)
import Prelude (Unit, bind, discard, map, pure, show, unit, ($), (<<<), (<>), (>>=))

-- Monadic handlers
logger :: Unit -> Handler
logger _ =
  getOriginalUrl >>=
    \url -> 
        (liftEffect $ log ("url = " <> url)) 
            -- \_ -> next
        

-- logger _ = do
--   url <- getOriginalUrl
--   liftEffect $ log (">>> " <> url)
--   next

-- email <- E.findEmail p
--     sendJson $ {email: email}

findEmailHandler :: Unit -> Handler
findEmailHandler _ = do
    mp <- getBodyParam "params"
    res <- fromMaybeToA mp (error "msg") >>= \(p :: E.EmailParams) -> E.findEmail p
    case res of
        Left e ->
            {result : "saryan bratan"}
        Right x ->
            case x of
                Just email ->
                    {result : show email}
                Nothing ->
                    {result : "no email"}
    sendJson $ {x: 33}


appSetup :: Unit -> App
appSetup _ = do
  liftEffect $ log "Setting up"
  setProp "json spaces" 4.0
  use (logger unit)
  post "/find-email" (findEmailHandler unit)
--   useOnError (errorHandler state)


fromMaybeToA :: forall e a. Maybe a -> e -> Aff (Either e a)
fromMaybeToA m e = pure (maybe (Left e) (\x -> Right x) m)
