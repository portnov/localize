{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, DeriveDataTypeable #-}

module Text.Localize.Load where

import Control.Monad
import qualified Data.Map as M
import qualified Data.Gettext as Gettext

import Text.Localize.Types

loadTranslations :: [(LanguageId, FilePath)] -> IO Translations
loadTranslations pairs = do
  res <- forM pairs $ \(lang, path) -> do
           gmo <- Gettext.loadCatalog path
           return (lang, gmo)
  return $ Translations $ M.fromList res

