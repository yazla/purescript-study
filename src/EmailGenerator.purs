module EmailGenerator where

import Data.Either

import AJAX (get)
import Data.List (List, fromFoldable)
import Data.Maybe (Maybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Debug.Trace (trace)
import Effect.Aff (Aff, Error)
import Foreign (unsafeFromForeign)
import ListAToA (findM)
import Milkis (URL(..), json, Response)
import Prelude (class Show, show, (<<<), (<>), (>), map, ($))
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

data VerifResult = VerifResult String

type VerificationInfo = {
     score :: Int,
     result :: VerifResult,
     score :: Int,
     email :: EmailAdress,
     regexp :: Boolean,
     gibberish :: Boolean,
     disposable :: Boolean,
     webmail :: Boolean,
     mx_records :: Boolean,
     smtp_server :: Boolean,
     smtp_check :: Boolean,
     accept_all :: Boolean,
     sources :: List String
  }

type VerificationResp = {
  data :: Nullable(VerificationInfo),
  errors :: Nullable (List EmailVerificationError)
}

emptyList :: List Error
emptyList = fromFoldable []

generate :: EmailParams -> List EmailAdress
generate p = fromFoldable [EmailAdress "xxx@linkmatch.net", EmailAdress "wwwyahoo.com", EmailAdress "yazla86@gmail.com"]


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
    transform = fromForeign transformVerificationResp


findEmail :: EmailParams -> Aff (Either Error (Maybe EmailAdress))
findEmail = findM verify <<< generate

transformVerificationResp :: VerificationResp -> Either Error Boolean
transformVerificationResp x = trace x \_ -> transform
  where
    transform = Right $ maybe default isVerifiedEmail verifInfoM
      where
          default = false
          verifInfoM = toMaybe x.data
          isVerifiedEmail = \verif_info -> verif_info.score > 70



-- (\errorsList ->
--   if length errorsList > 0
--   then Left (fromMaybe (error "Unknown error") (index errorsList 0))
--   else Right (trace x \_ -> x.data.score > 70)
-- )(nullableOrDefault emptyList x.errors)