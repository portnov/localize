{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, DeriveDataTypeable #-}

module Text.Localize where

import Prelude hiding (lookup)
import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Encoding as TE
import Data.String
import Data.List hiding (lookup)
import Data.Monoid
import Data.Typeable
import qualified Data.Gettext as Gettext
import qualified Data.Text.Format.Heavy as F
import Data.Text.Format.Heavy.Parse (parseFormat)

-- | Language identifier
type LanguageId = String

-- | Context name
type Context = B.ByteString

-- | String to be translated
type TranslationSource = B.ByteString

data Translations = Translations {
  tMap :: M.Map LanguageId Gettext.Catalog }

instance Show Translations where
  show t = "<Translations to languages: " ++ (unwords $ M.keys $ tMap t) ++ ">"

toText :: TranslationSource -> T.Text
toText bstr = T.fromStrict $ TE.decodeUtf8 bstr

lookup :: Translations -> LanguageId -> TranslationSource -> T.Text
lookup t lang bstr =
  case M.lookup lang (tMap t) of
    Nothing -> toText bstr
    Just gmo -> Gettext.gettext gmo bstr

loadTranslations :: [(LanguageId, FilePath)] -> IO Translations
loadTranslations pairs = do
  res <- forM pairs $ \(lang, path) -> do
           gmo <- Gettext.loadCatalog path
           return (lang, gmo)
  return $ Translations $ M.fromList res

class (Monad m, Applicative m) => Localized m where
  getLanguage :: m LanguageId
  getTranslations :: m Translations

  getContext :: m (Maybe Context)
  getContext = return Nothing

withTranslation :: Localized m => (b -> r) -> (Gettext.Catalog -> Maybe Context -> b -> r) -> (b -> m r)
withTranslation dflt fn b = do
  t <- getTranslations
  lang <- getLanguage
  ctxt <- getContext
  case M.lookup lang (tMap t) of
    Nothing -> return $ dflt b
    Just gmo -> return $ fn gmo ctxt b

translate :: (Localized m) => TranslationSource -> m T.Text
translate orig = do
  t <- getTranslations
  lang <- getLanguage
  mbContext <- getContext
  case M.lookup lang (tMap t) of
    Nothing -> return $ toText orig
    Just gmo ->
      case mbContext of
        Nothing -> return $ Gettext.gettext gmo orig
        Just ctxt -> return $ Gettext.cgettext gmo ctxt orig

__ ::  (Localized m) => TranslationSource -> m T.Text
__ = translate

translateN :: (Localized m) => TranslationSource -> TranslationSource -> Int -> m T.Text
translateN orig plural n = do
  t <- getTranslations
  lang <- getLanguage
  mbContext <- getContext
  case M.lookup lang (tMap t) of
    Nothing -> return $ toText orig
    Just gmo ->
      case mbContext of
        Nothing -> return $ Gettext.ngettext gmo orig plural n
        Just ctxt -> return $ Gettext.cngettext gmo ctxt orig plural n

translateFormat :: (Localized m, F.VarContainer c) => TranslationSource -> c -> m T.Text
translateFormat orig vars = do
  fmtStr <- translate orig
  case parseFormat fmtStr of
    Left err -> fail $ show err
    Right fmt -> return $ F.format fmt vars

__f ::  (Localized m, F.VarContainer c) => TranslationSource -> c -> m T.Text
__f = translateFormat

translateNFormat :: (Localized m, F.VarContainer c) => TranslationSource -> TranslationSource -> Int -> c -> m T.Text
translateNFormat orig plural n vars = do
  fmtStr <- translateN orig plural n
  case parseFormat fmtStr of
    Left err -> fail $ show err
    Right fmt -> return $ F.format fmt vars

__n :: (Localized m, F.VarContainer c) => TranslationSource -> TranslationSource -> Int -> c -> m T.Text
__n = translateNFormat

