{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, DeriveDataTypeable #-}

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

data Translations = Translations {
  tMap :: M.Map LanguageId Gettext.Catalog }

instance Show Translations where
  show t = "<Translations to languages: " ++ (unwords $ M.keys $ tMap t) ++ ">"

class (Monad m, Applicative m) => Localized m where
  getLanguage :: m LanguageId
  getTranslations :: m Translations

  getContext :: m (Maybe Context)
  getContext = return Nothing

toText :: TranslationSource -> T.Text
toText bstr = T.fromStrict $ TE.decodeUtf8 bstr

