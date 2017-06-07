
module Text.Localize.Locale where

import Data.Maybe
import System.Locale.SetLocale

import Text.Localize.Types
-- import Text.Localize.State

languageFromLocale :: IO LanguageId
languageFromLocale = do
  mbLocale <- setLocale LC_MESSAGES (Just "")
  let locale = fromMaybe "C" mbLocale
  return $ takeWhile (/= '_') locale

