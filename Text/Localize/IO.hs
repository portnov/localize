-- | This module is most near to be a drop-in replacement for @hgettext@ API.
-- It provides an @instance Localized IO@ by using process-level global variables (IORefs) for
-- storing current language and translations.
--
-- Being mostly an example, this module can though be usable for relatively simple applications,
-- for which you do not need to change languages a lot.
module Text.Localize.IO
  (setupTranslations,
   setLanguage, withLanguage,
   setContext, withContext
  ) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Text.Localize.Types
import Text.Localize.Load
import Text.Localize.Locale

currentContext :: IORef (Maybe Context)
currentContext = unsafePerformIO $ newIORef Nothing
{-# NOINLINE currentContext #-}

currentLanguage :: IORef LanguageId
currentLanguage = unsafePerformIO $ do
    language <- languageFromLocale
    newIORef language
{-# NOINLINE currentLanguage #-}

currentTranslations :: IORef Translations
currentTranslations = unsafePerformIO $ newIORef undefined
{-# NOINLINE currentTranslations #-}

instance Localized IO where
  getLanguage = readIORef currentLanguage
  getTranslations = readIORef currentTranslations
  getContext = readIORef currentContext

-- | This function must be called before any translation function call,
-- otherwise you will get runtime error.
--
-- Current language is selected from process locale at startup. You can
-- change it later by calling @setLanguage@ or @withLanguage@.
setupTranslations :: LocatePolicy -> IO ()
setupTranslations p = do
  translations <- locateTranslations p
  writeIORef currentTranslations translations

-- | Set current language.
setLanguage :: LanguageId -> IO ()
setLanguage language = writeIORef currentLanguage language

-- | Execute some actions with specific language, and
-- then return to previously used language.
withLanguage :: LanguageId -> IO a -> IO a
withLanguage language actions = do
  oldLang <- getLanguage
  setLanguage language
  result <- actions
  setLanguage oldLang
  return result

-- | Set current context.
setContext :: Maybe Context -> IO ()
setContext mbContext = writeIORef currentContext mbContext

-- | Execute some actions within specific context,
-- and then return to previously used context.
withContext :: Maybe Context -> IO a -> IO a
withContext ctxt actions = do
  oldContext <- getContext
  setContext ctxt
  result <- actions
  setContext oldContext
  return result

