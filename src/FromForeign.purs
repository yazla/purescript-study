module FromForeign where
import Data.Either
import Effect.Aff (Aff, attempt)
import Effect.Exception (Error)
import Foreign (unsafeFromForeign)
import Milkis (Response, json)
import Prelude (map, (<<<))

jsonFromForeign :: forall a b. (a -> b) -> Response -> Aff (Either Error b)
jsonFromForeign fn =
    attempt
    <<< map fn
    <<< map unsafeFromForeign
    <<< json