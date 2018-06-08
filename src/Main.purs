module Main where

import Data.Maybe

import Data.Options (Options, (:=))
import Effect (Effect)
import Effect.Console (logShow)
import Node.Buffer (toString)
import Node.Encoding (Encoding(..))
import Node.HTTP.Client as Client
import Node.Stream (end, onData, pipe, readString)
import Prelude (Unit, bind, discard, pure, unit, void, ($), (<>), (>>=))



testOpts :: Options Client.RequestOptions
testOpts = Client.protocol := "https:" <>
      Client.method := "GET" <>
      Client.hostname := "dou.ua" <>
      Client.path := "/" <>
      Client.rejectUnauthorized := false

y :: Maybe Int
y = Just 1000

main :: Effect Unit
main = do
  -- x <- y
  -- y >>= logShow
  req <- Client.request
    testOpts
    \r -> void do
      logShow $ Client.responseHeaders r
      let responseStream = Client.responseAsStream r

      onData
        responseStream
        \d -> toString UTF8 d >>= logShow

  end (Client.requestAsStream req) (pure unit)



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