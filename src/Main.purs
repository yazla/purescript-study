module Main where

import AJAX
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import EmailFinder as E
import Foreign (Foreign, unsafeFromForeign)
import Milkis (Fetch, Response, URL(..), defaultFetchOptions, json)
import Milkis.Impl.Node (nodeFetch)
import Prelude (Unit, bind, map, pure, show, ($), (<<<), unit, (<>))
import Node.HTTP (Server)
import Node.Express.App (listenHttp)
import LMEmailFinderServer(appSetup)
import Effect.Console (log)

main :: Effect Server
main = do
    listenHttp (appSetup unit) 8888 \_ ->
    log $ "Listening on " <> show "8888"


-- main = findEmailByParams


url :: URL
url = URL "https://api.hunter.io/v2/email-finder?company=Asana&full_name=Dustin+Moskovit&api_key=1d23c467945ddcf470c6d9d7a8e439515ceb1b7a"


p :: E.EmailParams
p = {
  first_name: "Anika",
  last_name: "Henry",
  middle_name: toNullable Nothing,
  company: (E.WebAddress "tr.com")
}

findEmailByParams :: Effect Unit
findEmailByParams = launchAff_ do
  response <- E.findEmail p
  liftEffect case response of
    Left e ->
      logShow(show e)
    Right x ->
      case x of
        Just email ->
          logShow email
        Nothing ->
          logShow "saryan"