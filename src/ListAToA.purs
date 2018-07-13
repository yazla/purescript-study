module ListAToA where

import Data.Either
import Prelude (class Monad, bind, pure)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))

findM :: forall a e m. Monad m => (a -> m (Either e Boolean)) -> List a -> m (Either e (Maybe a))
findM _ Nil = pure (Right Nothing)
findM pred (x : xs) = do
  b <- pred x
  case b of
    Left e ->
      pure (Left e)
    Right res ->
      if res
        then pure (Right (Just x))
        else findM pred xs

-- reduceM