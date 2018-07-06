module NullableAToA where


import Data.Maybe
import Data.Nullable (Nullable, toMaybe)
import Prelude


nullableOrDefault :: forall a b. a -> Nullable a -> a
nullableOrDefault d a =
    case toMaybe a of
        Just x ->
            x
        Nothing ->
            d