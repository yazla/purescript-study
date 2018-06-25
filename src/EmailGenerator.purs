module EmailGenerator where

import AJAX
import Data.Either
import ListAToA

import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Effect.Aff (Aff, Error)
import Foreign (Foreign, unsafeFromForeign)
import Milkis (URL(..), json, Response)
import Prelude (class Monad, class Show, bind, pure, show, ($), (<<<), (==), (<>), map)
import Type.Data.Boolean (kind Boolean)

data CompanyId = Name String | WebAddress String
data EmailAdress = EmailAdress String

instance showEmail :: Show EmailAdress where
  show (EmailAdress s) = show s 

type EmailParams = {
    first_name :: String,
    last_name :: String,
    middle_name :: Nullable String,
    company :: CompanyId
}

type VerifyResponse = {
  data :: {
     type :: String
  }
}

generate :: EmailParams -> List EmailAdress
generate p = fromFoldable [EmailAdress "xxx@dfdf.df", EmailAdress "www@sdfs.sdf", EmailAdress "yazla86@gmail.com"]


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
    transform = fromForeign (\x -> Right(x.data.type == "delivarable"))


findEmail :: EmailParams -> Aff (Maybe EmailAdress)
findEmail = findM verify <<< generate