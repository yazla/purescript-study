module EmailGenerator where

import AJAX
import Data.Array
import Data.Either
import Foreign.NullOrUndefined
import ListAToA

import Data.List (List(..), fromFoldable, (:), length, index)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable)
import Debug.Trace (spy, trace, traceM)
import Effect.Aff (Aff, Error, error)
import Foreign (Foreign, unsafeFromForeign)
import Milkis (URL(..), json, Response)
import NullableAToA (nullableOrDefault)
import Prelude (class Monad, class Show, bind, pure, show, ($), (<<<), (==), (/=), (<>), (>), map, (&&))
import Type.Data.Boolean (kind Boolean)

data CompanyId = Name String | WebAddress String
data EmailAdress = EmailAdress String

instance showEmail :: Show EmailAdress where
  show (EmailAdress s) = s 

type EmailParams = {
    first_name :: String,
    last_name :: String,
    middle_name :: Nullable String,
    company :: CompanyId
}

type EmailVerificationError = {}

type VerifyResponse = {
  data :: {
     score :: Int,
     type :: String
  },
  errors :: Nullable (List Error)
}

emptyList :: List Error
emptyList = fromFoldable []

generate :: EmailParams -> List EmailAdress
generate p = fromFoldable [EmailAdress "xxx@gmail.com", EmailAdress "wwwyahoo.com", EmailAdress "yazla86@gmail.com"]


createURL :: EmailAdress -> URL
createURL e = url
  where 
    url =  URL ("https://api.hunter.io/v2/email-verifier?email="<> show e <> "&api_key=1d23c467945ddcf470c6d9d7a8e439515ceb1b7a")


fromForeign :: forall a b. (b -> Either Error a) -> Response -> Aff (Either Error a)
fromForeign fn =
    map fn
    <<< map unsafeFromForeign
    <<< json

verify :: EmailAdress -> Aff (Either Error Boolean)
verify e = get transform url
  where
    url = createURL e
    transform = fromForeign
      \(x :: VerifyResponse) ->
        (\errorsList ->
          if length errorsList > 0
          then Left (fromMaybe (error "Unknown error") (index errorsList 0))
          else Right (trace x \_ -> x.data.score > 70)
        )(nullableOrDefault emptyList x.errors)


findEmail :: EmailParams -> Aff (Either Error (Maybe EmailAdress))
findEmail = findM verify <<< generate