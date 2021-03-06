module EmailFinder where

import Data.Either

import AJAX (get)
import Data.List (List)
import Data.Maybe (Maybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Debug.Trace (trace)
import Effect.Aff (Aff, Error)
import EmailGenerator as EmailGenerator
import EmailVerifier (verifyEmail, Result(..), Params)
import FromForeign (jsonFromForeign)
import ListAToA (findM)
import Milkis (URL(..))
import Prelude (class Eq, Unit, show, bind, pure, (&&), (<<<), (<>), (=<<), (==), (>), ($), (<$>))
import Type.Data.Boolean (kind Boolean, True)
import Type.Data.Symbol (SProxy)

data CompanyId = Name String | WebAddress String

type EmailParams = {
    first_name :: String,
    last_name :: String,
    middle_name :: Nullable String,
    company :: CompanyId
}

data ErrorCode = ErrorCode Int

type EmailVerificationError = {
  id :: String,
  code:: ErrorCode,
  details:: String
}

-- newtype OneOf (r :: # Type) = UnsafeOneOf String
-- pick :: forall s r l . IsSymbol s => Row.Cons s Unit l r => SProxy s -> OneOf r
-- pick s = UnsafeOneOf (reflectSymbol s)
-- unsafePick :: forall r . String -> OneOf r
-- unsafePick = UnsafeOneOf
-- match ...

data VerificationResult = Deliverable | NotDeliverable

derive instance eqVerResult :: Eq VerificationResult

type VerificationInfo = {
  score :: Int,
  result :: VerificationResult,
  email :: EmailGenerator.EmailAddress,
  regexp :: Boolean,
  gibberish :: Boolean,
  disposable :: Boolean,
  webmail :: Boolean,
  mx_records :: Boolean,
  smtp_server :: Boolean,
  smtp_check :: Boolean,
  accept_all :: Boolean,
  sources :: List String
}

type VerificationResponse = {
    score :: Int,
    result :: String,
    email :: EmailGenerator.EmailAddress,
    regexp :: Boolean,
    gibberish :: Boolean,
    disposable :: Boolean,
    webmail :: Boolean,
    mx_records :: Boolean,
    smtp_server :: Boolean,
    smtp_check :: Boolean,
    accept_all :: Boolean,
    sources :: List String
}

type VerificationResp = {
  data :: Nullable(VerificationResponse),
  errors :: Nullable (List EmailVerificationError)
}

createURL :: EmailGenerator.EmailAddress -> URL
createURL e = url
  where 
    url =  URL ("https://api.hunter.io/v2/email-verifier?email="<> show e <> "&api_key=1d23c467945ddcf470c6d9d7a8e439515ceb1b7a")


-- verify :: EmailGenerator.EmailAddress -> Aff (Either Error Boolean)
-- verify e = get transform url
--   where
--     url = createURL e
--     transform = jsonFromForeign verificationRespToBoolean


verify :: EmailGenerator.EmailAddress -> Aff (Either Error Boolean)
verify (EmailGenerator.EmailAddress e) = do
  result <- verifyEmail {
    helo : "google.com",
    from : "for.spam.only.yy@gmail.com",
    to : e,
    debug : true, -- default false
    catchalltest : true, -- default false
    timeout : 3000 -- default 5000
  }
  pure (Right $ result == EXIST)
        
convertResponse :: VerificationResponse -> VerificationInfo
convertResponse r = {
  score : r.score,
  result : toVerificationInfo(r.result),
  email : r.email,
  regexp : r.regexp,
  gibberish : r.gibberish,
  disposable : r.disposable,
  webmail : r.webmail,
  mx_records : r.mx_records,
  smtp_server : r.smtp_server,
  smtp_check : r.smtp_check,
  accept_all : r.accept_all,
  sources : r.sources
}

findEmail :: EmailParams -> Aff (Either Error (Maybe EmailGenerator.EmailAddress))
findEmail = findM verify <<< EmailGenerator.generateEmails <<< toGenerationParams

verificationRespToBoolean :: VerificationResp -> Boolean
verificationRespToBoolean x = trace x \_ -> 
  maybe default isVerifiedEmail verifInfoM
      where
          default = false
          isVerifiedEmail = \verif_info -> (verif_info.score > 70) && (verif_info.result == Deliverable)
          verifInfoM = convertResponse <$> toMaybe x.data



-- (\errorsList ->
--   if length errorsList > 0
--   then Left (fromMaybe (error "Unknown error") (index errorsList 0))
--   else Right (trace x \_ -> x.data.score > 70)
-- )(nullableOrDefault emptyList x.errors)

toGenerationParams :: EmailParams -> EmailGenerator.EmailParams
toGenerationParams p = {
    first_name: p.first_name,
    last_name: p.last_name,
    company_web: case p.company of
      Name s -> s
      WebAddress s -> s
  }

toVerificationInfo :: String -> VerificationResult
toVerificationInfo "deliverable" = Deliverable
toVerificationInfo _ = NotDeliverable