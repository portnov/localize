{-# LANGUAGE TypeSynonymInstances, GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Text.Localize.State where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans

import Text.Localize.Types

data LocState = LocState {
    lsTranslations :: Translations, 
    lsLanguage :: LanguageId,
    lsContext :: Maybe Context
  }
  deriving (Show)

newtype LocalizeT m a = LocalizeT {
    unLocalizeT :: StateT LocState m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState LocState)

instance Monad m => Localized (LocalizeT m) where
  getTranslations = gets lsTranslations
  getLanguage = gets lsLanguage
  getContext = gets lsContext

runLocalizeT :: Monad m => LocalizeT m a -> LocState -> m a
runLocalizeT actions st = evalStateT (unLocalizeT actions) st

setLanguage :: Monad m => LanguageId -> LocalizeT m ()
setLanguage lang = modify $ \st -> st {lsLanguage = lang}

withLanguage :: Monad m => LanguageId -> LocalizeT m a -> LocalizeT m a
withLanguage lang actions = do
  oldLang <- gets lsLanguage
  setLanguage lang
  result <- actions
  setLanguage oldLang
  return result

setContext :: Monad m => Maybe Context -> LocalizeT m ()
setContext ctxt = modify $ \st -> st {lsContext = ctxt}

withContext :: Monad m => Maybe Context -> LocalizeT m a -> LocalizeT m a
withContext ctxt actions = do
  oldContext <- gets lsContext
  setContext ctxt
  result <- actions
  setContext oldContext
  return result


