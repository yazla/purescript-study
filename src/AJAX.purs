module AJAX where
    
import Data.Either (Either(..))
import Effect.Aff (Aff, Error, attempt)
import Milkis (Fetch, Response, URL, defaultFetchOptions, fetch)
import Milkis.Impl.Node (nodeFetch)
import Prelude (bind, pure, ($))

n_fetch :: Fetch
n_fetch = fetch nodeFetch

get :: forall a. (Response -> Aff (Either Error a)) -> URL -> Aff (Either Error a)
get resp_parser url = do
    r <- attempt $ n_fetch url defaultFetchOptions
    case r of
        Left err ->
            pure (Left err)
        Right res ->
            resp_parser res