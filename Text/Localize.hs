{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Text.Localize where

import Prelude hiding (lookup)
import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Trie as Trie
import Data.String
import Text.Printf

import Data.Gmo

import Debug.Trace

type LanguageId = String

data Translations = Translations {
  tMap :: M.Map LanguageId GmoData }
  deriving (Eq)

instance Show Translations where
  show t = printf "<Translations to languages: %s>" (unwords $ M.keys $ tMap t)

data LocalizedString = LocalizedString {
    mlAsByteString :: B.ByteString,
    mlAsText :: T.Text }
  deriving (Eq)

instance Show LocalizedString where
  show ml = T.unpack $ mlAsText ml

class IsLocalizedString str where
  __ :: str -> LocalizedString

instance IsLocalizedString T.Text where
  __ text = LocalizedString {
              mlAsByteString = L.toStrict $ TLE.encodeUtf8 text,
              mlAsText = text }

instance IsLocalizedString B.ByteString where
  __ bstr = LocalizedString {
              mlAsByteString = bstr,
              mlAsText = TLE.decodeUtf8 $ L.fromStrict bstr }

instance IsLocalizedString String where
  __ str = __ (T.pack str)

instance IsString LocalizedString where
  fromString str = __ str

lookup :: LanguageId -> B.ByteString -> Int -> Translations -> Maybe T.Text
lookup lang bstr i t = do
  gmo <- M.lookup lang (tMap t)
  list <- Trie.lookup bstr (gmoData gmo)
  if i >= length list
    then Nothing
    else return $ list !! i

loadTranslations :: [(LanguageId, FilePath)] -> IO Translations
loadTranslations pairs = do
  res <- forM pairs $ \(lang, path) -> do
           gmo <- withGmoFile path (return . unpackGmoFile)
           return (lang, gmo)
  return $ Translations $ M.fromList res

class Monad m => Localized m where
  getLanguage :: m LanguageId
  getTranslations :: m Translations

getText :: (Localized m) => B.ByteString -> m T.Text
getText bstr = do
  t <- getTranslations
  lang <- getLanguage
  case lookup lang bstr 0 t of
    Nothing -> return $ TLE.decodeUtf8 $ L.fromStrict bstr
    Just res -> return res

translate :: Localized m => LocalizedString -> m T.Text
translate lstr = do
  getText (mlAsByteString lstr)

