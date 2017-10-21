{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, DeriveDataTypeable #-}
-- | This module contains data type definitions for the @localize@ package.
module Text.Localize.Types where

import Control.Applicative
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as TE
import qualified Data.Gettext as Gettext

-- | Language identifier
type LanguageId = String

-- | Context name
type Context = B.ByteString

-- | String to be translated
type TranslationSource = B.ByteString

-- | Stores translation catalogs for all supported languages
data Translations = Translations {
  tMap :: M.Map LanguageId Gettext.Catalog }

instance Show Translations where
  show t = "<Translations to languages: " ++ (unwords $ M.keys $ tMap t) ++ ">"

-- | This is the main type class of the package.
-- All functions work with any instance of this type class.
-- 
-- Note that this class only supports **getting** current language
-- and translation, not setting them. Though concrete implementation
-- can have its own ways to change language or context, these ways are
-- not required by the @localize@ package, and so are not declared in the
-- type class.
class (Monad m, Applicative m) => Localized m where
  -- | Obtain currently selected language ID.
  getLanguage :: m LanguageId
  
  -- | Obtain currently loaded translations.
  getTranslations :: m Translations

  -- | Obtain currently selected localization context.
  -- Nothing means no specific context.
  getContext :: m (Maybe Context)
  getContext = return Nothing

-- | This assumes UTF-8 encoding.
toText :: TranslationSource -> T.Text
toText bstr = T.fromStrict $ TE.decodeUtf8 bstr

