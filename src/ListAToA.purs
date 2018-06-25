module ListAToA where

import Prelude (class Monad, bind, pure)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))

findM :: forall a m. Monad m => (a -> m Boolean) -> List a -> m (Maybe a)
findM _ Nil = pure (Nothing)
findM pred (x : xs) = do
  b <- pred x
  if b
    then pure(Just x)
    else findM pred xs