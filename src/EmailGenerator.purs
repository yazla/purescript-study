module EmailGenerator where

import Prelude

import Data.List (List, fromFoldable)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.String.CodeUnits (take)

data EmailTemplate = EmailTemplate String

data EmailAddress = EmailAddress String

instance showEmail :: Show EmailAddress where
  show (EmailAddress s) = s 

type EmailParams = {
    first_name :: String,
    last_name :: String,
    company_web :: String
}

generateEmails :: EmailParams -> List EmailAddress
generateEmails e = applyTemplate e <$> emailTemplates

emailTemplates :: List EmailTemplate
emailTemplates = fromFoldable [
  EmailTemplate "${first_name}_${last_name}@${company_web}",
  EmailTemplate "${first_name}.${last_name}@${company_web}",
  EmailTemplate "${first_name}-${last_name}@${company_web}",
  EmailTemplate "${last_name}@${company_web}",
  EmailTemplate "${first_name1}${last_name}@${company_web}",
  EmailTemplate "${last_name}${first_name1}@${company_web}",
  EmailTemplate "${first_name1}.${last_name}@${company_web}",
  EmailTemplate "${first_name1}-${last_name}@${company_web}",
  EmailTemplate "${first_name1}_${last_name}@${company_web}",
  EmailTemplate "${first_name}${last_name1}@${company_web}",
  EmailTemplate "${first_name}.${last_name1}@${company_web}",
  EmailTemplate "${first_name}-${last_name1}@${company_web}",
  EmailTemplate "${first_name}_${last_name1}@${company_web}",
  EmailTemplate "${first_name}${last_name}@${company_web}",
  EmailTemplate "${last_name}${first_name}@${company_web}",
  EmailTemplate "${last_name}.${first_name}@${company_web}",
  EmailTemplate "${last_name}_${first_name}@${company_web}",
  EmailTemplate "${first_name}@${company_web}"
]

applyTemplate :: EmailParams -> EmailTemplate -> EmailAddress
applyTemplate p (EmailTemplate t) =
    EmailAddress (
        replace (Pattern "${first_name}") (Replacement p.first_name)
        $
        replace (Pattern "${first_name1}") (Replacement (take 1 p.first_name))
        $
        replace (Pattern "${last_name}") (Replacement p.last_name)
        $
        replace (Pattern "${last_name1}") (Replacement (take 1 p.last_name))
        $
        replace (Pattern "${company_web}") (Replacement p.company_web) t
    )