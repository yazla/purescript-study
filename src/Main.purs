module Main where

import Data.Maybe (Maybe(..))

import Data.Options (Options, (:=))
import Effect (Effect)
import Effect.Aff (runAff_, attempt, launchAff_, Aff)
import Effect.Console (logShow)
import Effect.Class
import Effect.Exception
import Milkis (URL(..), defaultFetchOptions, Fetch, Response)
import Milkis as M
import Milkis.Impl.Node
import Node.HTTP.Client as Client
import Prelude
import Data.Either (Either(..))
import Foreign


type PersonResponse = {
  data :: {
    first_name :: String
  }
}

fetch :: Fetch
fetch = M.fetch nodeFetch

url :: URL
url = URL "https://api.hunter.io/v2/email-finder?company=Asana&full_name=Dustin+Moskovit&api_key=1d23c467945ddcf470c6d9d7a8e439515ceb1b7a"

getPerson :: Foreign -> PersonResponse
getPerson = unsafeFromForeign

responseToEitherAffPerson :: Response -> Aff (Either Error PersonResponse)
responseToEitherAffPerson =
  map (\x -> Right x) <<< map(getPerson) <<< M.json

-- errorTo :: Error -> Either Error PersonResponse
-- errorTo x = 

getResponse :: Either Error Response -> Aff (Either Error PersonResponse)
getResponse r = do
  case r of
    Left e ->
      pure (Left e)
    Right res ->
      responseToEitherAffPerson res

main :: Effect Unit
main = launchAff_ do
  r <- attempt $ fetch url defaultFetchOptions
  response <- getResponse(r)
  liftEffect case response of
    Left e ->
      logShow("error")
    Right res ->
      logShow res.data.first_name
  -- runAff_ (\x -> case x of
  --   Left e -> do
  --     logShow $ pure "error"
  --   Right res -> do
  --     t <- M.text res
  --     logShow t
  --       )(fetch url defaultFetchOptions)


      
-- either_test = getUserE
--                 >>> map getUserName
--                 >>> unsafePartial (fromRight)

-- logShow (foldl(\a x -> a + x + 10.0) 0.0 [1.0, 2.0, 3.0])

-- plus5 :: Int -> Int
-- plus5 x = x + 5

-- add5 :: Number -> Number
-- add5 n = n + 5.0

-- https://github.com/slamdata/purescript-affjax

-- average :: Fold Number Number
-- average = (/) <$> sum <*> length

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- main = logShow (map add5 [1.0, 2.0, 3.0])