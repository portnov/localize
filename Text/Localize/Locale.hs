-- | This module contains definitions related to
-- obtaining current localization settings from
-- process's locale.
module Text.Localize.Locale where

import Data.Maybe
import System.Locale.SetLocale

import Text.Localize.Types

-- | Obtain language to be used from process's locale.
languageFromLocale :: IO LanguageId
languageFromLocale = do
  mbLocale <- setLocale LC_MESSAGES (Just "")
  let locale = fromMaybe "C" mbLocale
  return $ takeWhile (/= '_') locale

