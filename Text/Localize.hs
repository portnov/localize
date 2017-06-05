{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, DeriveDataTypeable #-}

module Text.Localize
  (
   -- * Most used functions
   __, __n, __f,
   -- * Basic functions
   translate, translateN, translateNFormat, 
   lookup,
   -- * Reexports
   module Text.Localize.Types,
   module Text.Localize.Load
  ) where

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

import Text.Localize.Types
import Text.Localize.Load

-- | Look up for translation. Returns source string if translation not found.
lookup :: Translations -> LanguageId -> TranslationSource -> T.Text
lookup t lang bstr =
  case M.lookup lang (tMap t) of
    Nothing -> toText bstr
    Just gmo -> Gettext.gettext gmo bstr

withTranslation :: Localized m => (b -> r) -> (Gettext.Catalog -> Maybe Context -> b -> r) -> (b -> m r)
withTranslation dflt fn b = do
  t <- getTranslations
  lang <- getLanguage
  ctxt <- getContext
  case M.lookup lang (tMap t) of
    Nothing -> return $ dflt b
    Just gmo -> return $ fn gmo ctxt b

-- | Translate a string.
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

-- | Short alias for @translate@.
__ ::  (Localized m) => TranslationSource -> m T.Text
__ = translate

-- | Translate a string, taking plural forms into account.
translateN :: (Localized m)
           => TranslationSource -- ^ Single form in original language
           -> TranslationSource -- ^ Plural form in original language
           -> Int               -- ^ Number
           -> m T.Text
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

-- | Translate a string and substitute variables into it.
-- Data.Text.Format.Heavy.format syntax is used.
translateFormat :: (Localized m, F.VarContainer vars)
                => TranslationSource -- ^ Original formatting string
                -> vars              -- ^ Substitution variables
                -> m T.Text
translateFormat orig vars = do
  fmtStr <- translate orig
  case parseFormat fmtStr of
    Left err -> fail $ show err
    Right fmt -> return $ F.format fmt vars

-- | Short alias for @translateFormat@.
__f ::  (Localized m, F.VarContainer c) => TranslationSource -> c -> m T.Text
__f = translateFormat

-- | Translate a string, taking plural forms into account,
-- and substitute variables into it.
-- Data.Text.Format.Heavy.format syntax is used.
translateNFormat :: (Localized m, F.VarContainer vars)
                 => TranslationSource -- ^ Single form of formatting string in original language
                 -> TranslationSource -- ^ Plural form of formatting string in original language
                 -> Int               -- ^ Number
                 -> vars              -- ^ Substitution variables
                 -> m T.Text
translateNFormat orig plural n vars = do
  fmtStr <- translateN orig plural n
  case parseFormat fmtStr of
    Left err -> fail $ show err
    Right fmt -> return $ F.format fmt vars

-- | Short alias for @translateNFormat@.
__n :: (Localized m, F.VarContainer c) => TranslationSource -> TranslationSource -> Int -> c -> m T.Text
__n = translateNFormat

