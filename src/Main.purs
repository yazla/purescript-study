module Main where

import AJAX
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import EmailGenerator(findEmail, WebAddress, EmailParams)
import Foreign (Foreign, unsafeFromForeign)
import Milkis (Fetch, Response, URL(..), defaultFetchOptions, json)
import Milkis.Impl.Node (nodeFetch)
import Prelude (Unit, bind, map, pure, show, ($), (<<<))

main :: Effect Unit
main = findEmailByParams

type PersonResponse = {
  data :: {
    first_name :: String,
    email :: String
  }
}


url :: URL
url = URL "https://api.hunter.io/v2/email-finder?company=Asana&full_name=Dustin+Moskovit&api_key=1d23c467945ddcf470c6d9d7a8e439515ceb1b7a"

responseToPersonPesponse :: Response -> Aff (Either Error PersonResponse)
responseToPersonPesponse = map Right <<< map unsafeFromForeign <<< json


getResponse :: Either Error Response -> Aff (Either Error PersonResponse)
getResponse r = do
  case r of
    Left e ->
      pure (Left e)
    Right res ->
      responseToPersonPesponse res

p :: EmailParams
p = {
  first_name: "Yuriy",
  last_name: "Yazlovytskyy",
  middle_name: toNullable Nothing,
  company: (WebAddress "linkmatch.net")
}
findEmailByParams :: Effect Unit
findEmailByParams = launchAff_ do
  response <- findEmail p
  liftEffect case response of
    Just e ->
      logShow(show e)
    Nothing ->
      logShow "saryan"


findEmail :: Effect Unit
findEmail = launchAff_ do
  r <- get url
  response <- getResponse(r)
  liftEffect case response of
    Left e ->
      logShow("error")
    Right res ->
      logShow res.data.email