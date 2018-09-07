module EmailVerifier
  ( verifyEmail, Result(..), Params)
  where

import Prelude (class Eq, ($), map)
import Effect.Aff (Aff)
import Control.Promise (Promise, toAff)

data Result = 
    INVALID -- email regexp validation failed
    | EXIST -- email is existence
    | NOT_EXIST -- email is not existence
    | CATCH_ALL -- catch all smtp server
    | MXRECORD_TIMEOUT -- resolve mx record timeout
    | MXRECORD_FAIL -- resolve mx record fail
    | CONN_FAIL -- connect fail smtp
    | CONN_TIMEOUT -- connect timeout smtp
    | VERIFY_TIMEOUT
    | VERIFY_FAIL
    | UNKNOWN

derive instance eqResult :: Eq Result

type Params = {
  helo :: String,
  from :: String,
  to :: String,
  debug :: Boolean, -- default false
  catchalltest :: Boolean, -- default false
  timeout :: Int -- default 5000
}

foreign import verifyEmail_ :: Params -> Promise String

stringToResult :: String -> Result
stringToResult s = 
  case s of 
    "INVALID" -> INVALID
    "EXIST" -> EXIST
    "NOT_EXIST" -> NOT_EXIST
    "CATCH_ALL" -> CATCH_ALL
    "MXRECORD_TIMEOUT" -> MXRECORD_TIMEOUT
    "MXRECORD_FAIL" -> MXRECORD_FAIL
    "CONN_FAIL" -> CONN_FAIL
    "CONN_TIMEOUT" -> CONN_TIMEOUT
    "VERIFY_TIMEOUT" -> VERIFY_TIMEOUT
    "VERIFY_FAIL" -> VERIFY_FAIL
    _ -> UNKNOWN



verifyEmail :: Params -> Aff Result
verifyEmail p = map stringToResult (toAff(verifyEmail_ p))

