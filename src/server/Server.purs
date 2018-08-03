module LMEmailFinderServer(appSetup) where
import Data.Either
import Prelude hiding (apply)

import Data.Function.Uncurried (Fn3)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toNullable, Nullable)
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


type RequestParams = {
    first_name :: String,
    last_name :: String,
    middle_name :: Nullable String,
    company :: String
}

toEmailParams :: RequestParams -> E.EmailParams
toEmailParams p = {
    first_name : p.first_name,
    last_name : p.last_name,
    middle_name : p.middle_name,
    company : E.WebAddress p.company
}
        
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
    (mp :: Maybe RequestParams) <- getBodyParam "params"    
    case mp of
        Just p -> do
            result <- liftAff $ getEmail $ toEmailParams p
            sendJson $ {result: result}
        Nothing ->
            sendJson $ { result : "missing email params" }
    

getAff :: Aff String
getAff = pure "kolya"

appSetup :: Unit -> App
appSetup _ = do
  liftEffect $ log "Setting up"
  useExternal jsonBodyParser
  setProp "json spaces" 4.0
  use (logger unit)
  post "/find-email" (findEmailHandler unit)
--   useOnError (errorHandler state)