{-# LANGUAGE TypeSynonymInstances, GeneralizedNewtypeDeriving, OverloadedStrings #-}
-- | This module offers a simple implementation of @Localized@ class via @StateT@ transformer.
-- This implementation is usable if you have nothing against adding yet another transformer to
-- your already complicated monadic stack. Otherwise, it may be simpler for you to add necessary
-- fields to one of @StateT@s or @ReaderT@s in your stack.
module Text.Localize.State
  (-- * Data types
   LocState (..),
   LocalizeT (..),
   -- * Functions
   runLocalizeT,
   setLanguage, withLanguage,
   setContext, withContext
  ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans

import Text.Localize.Types

-- | Localization state
data LocState = LocState {
    lsTranslations :: Translations, 
    lsLanguage :: LanguageId,
    lsContext :: Maybe Context
  }
  deriving (Show)

-- | Localization monad transformer
newtype LocalizeT m a = LocalizeT {
    unLocalizeT :: StateT LocState m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState LocState)

instance Monad m => Localized (LocalizeT m) where
  getTranslations = gets lsTranslations
  getLanguage = gets lsLanguage
  getContext = gets lsContext

-- | Run a computation inside @LocalizeT@.
runLocalizeT :: Monad m => LocalizeT m a -> LocState -> m a
runLocalizeT actions st = evalStateT (unLocalizeT actions) st

-- | Set current language
setLanguage :: Monad m => LanguageId -> LocalizeT m ()
setLanguage lang = modify $ \st -> st {lsLanguage = lang}

-- | Execute some actions with specified language.
withLanguage :: Monad m => LanguageId -> LocalizeT m a -> LocalizeT m a
withLanguage lang actions = do
  oldLang <- gets lsLanguage
  setLanguage lang
  result <- actions
  setLanguage oldLang
  return result

-- | Set current context.
setContext :: Monad m => Maybe Context -> LocalizeT m ()
setContext ctxt = modify $ \st -> st {lsContext = ctxt}

-- | Execute some actions within specific context.
withContext :: Monad m => Maybe Context -> LocalizeT m a -> LocalizeT m a
withContext ctxt actions = do
  oldContext <- gets lsContext
  setContext ctxt
  result <- actions
  setContext oldContext
  return result

