module LMEmailFinderServer(appSetup) where
import Data.Either
import Prelude hiding (apply)

import Data.Function.Uncurried (Fn3)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toNullable)
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import EmailFinder as E
import EmailGenerator as EmailGenerator
import Node.Express.App (App, listenHttp, useOnError, get, post, use, setProp, useExternal)
import Node.Express.Handler (Handler, HandlerM(..), next, nextThrow)
import Node.Express.Request (getRouteParam, getQueryParam, getOriginalUrl, setUserData, getUserData, getBodyParam)
import Node.Express.Response (sendJson, setStatus)
import Node.Express.Types (Response, Request)
import Prelude (Unit, bind, discard, map, pure, show, unit, ($), (<<<), (<>), (>>=))

foreign import jsonBodyParser :: Fn3 Request Response (Effect Unit) (Effect Unit)

        
logger :: Unit -> Handler
logger _ = do
  url <- getOriginalUrl
  liftEffect $ log ("url >>> " <> url)
  next

getEmail :: E.EmailParams ->  Aff {result :: String}
getEmail p = do
    r <- E.findEmail p
    case r of
        Left e ->
            pure {result : "error"}
        Right em ->
            case em of
                Just email ->
                    pure {result : show email}
                Nothing ->
                    pure {result : "no email found"}

findEmailHandler :: Unit -> Handler
findEmailHandler _ = do
    (mp :: Maybe E.EmailParams) <- getBodyParam "params"
    case mp of
        Just p ->
            sendJson $ (getEmail p)
        Nothing ->
            sendJson $ {result : "missing email params"}


appSetup :: Unit -> App
appSetup _ = do
  liftEffect $ log "Setting up"
  useExternal jsonBodyParser
  setProp "json spaces" 4.0
  use (logger unit)
  post "/find-email" (findEmailHandler unit)
--   useOnError (errorHandler state)