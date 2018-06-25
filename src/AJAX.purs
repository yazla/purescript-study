module AJAX where
    
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (logShow)
import Foreign (Foreign, unsafeFromForeign)
import Milkis (Fetch, Response, URL(..), defaultFetchOptions, fetch, json)
import Milkis.Impl.Node (nodeFetch)
import Prelude (Unit, bind, map, pure, show, ($), (<<<))

n_fetch :: Fetch
n_fetch = fetch nodeFetch

get :: forall a. (Response -> Aff (Either Error a)) -> URL -> Aff (Either Error a)
get resp_parser url = do
    r <- attempt $ n_fetch url defaultFetchOptions
    case r of
        Left e ->
            pure (Left e)
        Right res ->
            resp_parser res