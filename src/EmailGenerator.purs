module Email where

import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Effect.Aff (Aff)

import Prelude (class Monad, class Show, bind, pure, show, ($), (<<<), (==))

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

generate :: EmailParams -> List EmailAdress
generate p = fromFoldable [EmailAdress "xxx@dfdf.df", EmailAdress "www@sdfs.sdf"]

verify :: EmailAdress -> Aff Boolean
verify (EmailAdress e) = pure (e == "www@sdfs.sdf")

findEmail :: EmailParams -> Aff (Maybe EmailAdress)
findEmail = (findM $ verify) <<< generate

findM :: forall a m. Monad m => (a -> m Boolean) -> List a -> m (Maybe a)
findM _ Nil = pure Nothing
findM p (x : xs) = do
  b <- p x
  if b then pure(Just x) else findM p xs