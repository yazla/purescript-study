module Main where

import Effect.Class (liftEffect)
import Milkis.Impl.Node (nodeFetch)
import Prelude (Unit, bind, show)

import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (logShow)
import Email as E
import Milkis (Fetch)
import Milkis as M

type PersonResponse = {
  data :: {
    first_name :: String
  }
}

fetch :: Fetch
fetch = M.fetch nodeFetch

-- url :: URL
-- url = URL "https://api.hunter.io/v2/email-finder?company=Asana&full_name=Dustin+Moskovit&api_key=1d23c467945ddcf470c6d9d7a8e439515ceb1b7a"

-- getPerson :: Foreign -> PersonResponse
-- getPerson = unsafeFromForeign

-- responseToAffEitherPerson :: Response -> Aff (Either Error PersonResponse)
-- responseToAffEitherPerson =
--   map (\x -> Right x) <<< map(getPerson) <<< M.json

-- -- errorTo :: Error -> Either Error PersonResponse
-- -- errorTo x = 

-- getResponse :: Either Error Response -> Aff (Either Error PersonResponse)
-- getResponse r = do
--   case r of
--     Left e ->
--       pure (Left e)
--     Right res ->
--       responseToEitherAffPerson res

p :: E.EmailParams
p = {
  first_name: "Yuriy",
  last_name: "Yazlovytskyy",
  middle_name: toNullable Nothing,
  company: (E.WebAddress "linkmatch.net")
}


main :: Effect Unit
main = launchAff_ do
  response <- E.findEmail p
  liftEffect case response of
    Just e ->
      logShow(show e)
    Nothing ->
      logShow "saryan"



-- main :: Effect Unit
-- main = launchAff_ do
--   r <- attempt $ fetch url defaultFetchOptions
--   response <- getResponse(r)
--   liftEffect case response of
--     Left e ->
--       logShow("error")
--     Right res ->
--       logShow res.data.first_name
      
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