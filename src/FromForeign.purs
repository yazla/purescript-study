module FromForeign where
import Milkis (Response, json)
import Effect.Aff (Aff)
import Prelude((<<<), map)
import Foreign (unsafeFromForeign)

jsonFromForeign :: forall a b. (a -> b) -> Response -> Aff b
jsonFromForeign fn =
    map fn
    <<< map unsafeFromForeign
    <<< json