module FromForeign where
import Milkis (Response, json)
import Effect.Aff (Aff)
import Prelude((<<<), map)
import Foreign (unsafeFromForeign)

fromForeign :: forall a b. (a -> b) -> Response -> Aff b
fromForeign fn =
    map fn
    <<< map unsafeFromForeign
    <<< json