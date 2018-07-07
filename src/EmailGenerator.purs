module EmailGenerator where

import AJAX
import Data.Array
import Data.Either
import Foreign.NullOrUndefined
import ListAToA

import Data.Int.Bits (xor)
import Data.List (List(..), fromFoldable, (:), length, index)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable)
import Data.Nullable (toNullable, toMaybe)
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

data ErrorCode = ErrorCode Int

type EmailVerificationError = {
  id :: String,
  code:: ErrorCode,
  details:: String
}

type ScoreInfo = {
     score :: Int,
     type :: String
  }

type VerifyResponse = {
  data :: Nullable(ScoreInfo),
  errors :: Nullable (List EmailVerificationError)
}

emptyList :: List Error
emptyList = fromFoldable []

generate :: EmailParams -> List EmailAdress
generate p = fromFoldable [EmailAdress "xxx@gmail.com", EmailAdress "wwwyahoo.com", EmailAdress "yazla86@gmail.com"]


createURL :: EmailAdress -> URL
createURL e = url
  where 
    url =  URL ("https://api.hunter.io/v2/email-verifier?email="<> show e <> "&api_key=1d23c467945ddcf470c6d9d7a8e439515ceb1b7a")


fromForeign :: forall a b e. (b -> Either e a) -> Response -> Aff (Either e a)
fromForeign fn =
    map fn
    <<< map unsafeFromForeign
    <<< json

verify :: EmailAdress -> Aff (Either Error Boolean)
verify e = get transform url
  where
    url = createURL e
    transform = fromForeign transformVerifyResponse


findEmail :: EmailParams -> Aff (Either Error (Maybe EmailAdress))
findEmail = findM verify <<< generate

transformVerifyResponse :: VerifyResponse -> Either Error Boolean
transformVerifyResponse x = trace x \_x -> maybe (Right false) checkScore (toMaybe(x.data))

checkScore :: ScoreInfo -> Either Error Boolean
checkScore x = Right (x.score > 70)
-- Right (trace x \_ -> x.data.score > 70)


      -- (\errorsList ->
      --   if length errorsList > 0
      --   then Left (fromMaybe (error "Unknown error") (index errorsList 0))
      --   else Right (trace x \_ -> x.data.score > 70)
      -- )(nullableOrDefault emptyList x.errors)