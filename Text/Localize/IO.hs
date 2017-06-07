
module Text.Localize.IO where

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
setupTranslations :: LocatePolicy -> IO ()
setupTranslations p = do
  translations <- locateTranslations p
  writeIORef currentTranslations translations

setLanguage :: LanguageId -> IO ()
setLanguage language = writeIORef currentLanguage language

withLanguage :: LanguageId -> IO a -> IO a
withLanguage language actions = do
  oldLang <- getLanguage
  setLanguage language
  result <- actions
  setLanguage oldLang
  return result

setContext :: Maybe Context -> IO ()
setContext mbContext = writeIORef currentContext mbContext

withContext :: Maybe Context -> IO a -> IO a
withContext ctxt actions = do
  oldContext <- getContext
  setContext ctxt
  result <- actions
  setContext oldContext
  return result

