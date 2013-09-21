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
import qualified Data.Trie as Trie
import Data.String
import Text.Printf
import qualified Data.Text.Format as Format
import qualified Data.Text.Format.Params as Params
import qualified Data.Text.Format.Types.Internal as I
import Data.List hiding (lookup)
import Data.Monoid
import Data.Typeable

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
        mlAsText :: T.Text,
        mlParams :: Params }
     | Append LocalizedString LocalizedString
     | Untranslated T.Text
  deriving (Typeable)

instance Eq LocalizedString where
  (Untranslated t1) == (Untranslated t2) = t1 == t2
  (Append l1 l2) == (Append l3 l4) =
      (l1 == l3) && (l2 == l4)
  l1 == l2 = mlAsByteString l1 == mlAsByteString l2

instance Show LocalizedString where
  show (Untranslated t) = T.unpack t
  show (Append l1 l2) = show l1 ++ show l2
  show ml = T.unpack $ mlAsText ml

instance Monoid LocalizedString where
--   mempty = LocalizedString B.empty T.empty (P ())
  mempty = Untranslated (T.pack "")
  mappend = Append

data Params = forall ps. Params.Params ps => P ps

class IsLocalizedString str where
  __ :: str -> LocalizedString

instance IsLocalizedString T.Text where
  __ text = LocalizedString {
              mlAsByteString = L.toStrict $ TLE.encodeUtf8 text,
              mlAsText = text,
              mlParams = P () }

instance IsLocalizedString B.ByteString where
  __ bstr = LocalizedString {
              mlAsByteString = bstr,
              mlAsText = TLE.decodeUtf8 $ L.fromStrict bstr,
              mlParams = P () }

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

class (Monad m, Applicative m) => Localized m where
  getLanguage :: m LanguageId
  getTranslations :: m Translations

getText :: (Localized m) => B.ByteString -> m T.Text
getText bstr = do
  t <- getTranslations
  lang <- getLanguage
  case lookup lang bstr 0 t of
    Nothing -> return $ TLE.decodeUtf8 $ L.fromStrict bstr
    Just res -> return res

translateNoformat' :: Translations -> LanguageId -> LocalizedString -> T.Text
translateNoformat' t lang lstr =
  case lookup lang (mlAsByteString lstr) 0 t of
    Nothing -> mlAsText lstr
    Just res -> res

translateNoformat :: Localized m => LocalizedString -> m T.Text
translateNoformat (Untranslated t) = return t
translateNoformat (Append l1 l2) = T.append <$> translateNoformat l1 <*> translateNoformat l2
translateNoformat lstr = do
  t <- getTranslations
  lang <- getLanguage
  return $ translateNoformat' t lang lstr

translate :: Localized m =>  LocalizedString -> m T.Text
translate (Untranslated t) = return t
translate (Append l1 l2) = T.append <$> translate l1 <*> translate l2
translate lstr@(LocalizedString _ _ (P params)) = do
  formatText <- translateNoformat lstr
  let fmt = I.Format $ T.toStrict formatText
  return $ Format.format fmt params

translate' :: Translations -> LanguageId -> LocalizedString -> T.Text
translate' _ _ (Untranslated t) = t
translate' t lang (Append l1 l2) = translate' t lang l1 `T.append` translate' t lang l2
translate' t lang lstr@(LocalizedString _ _ (P params)) = 
  let formatText = translateNoformat' t lang lstr
      fmt = I.Format $ T.toStrict formatText
  in  Format.format fmt params

lprintf :: (IsLocalizedString str, Params.Params ps) => str -> ps -> LocalizedString
lprintf str ps = (__ str) {mlParams = P ps}

unlinesL :: [LocalizedString] -> LocalizedString
unlinesL list = mconcat $ intersperse (Untranslated $ T.pack "\n") list

unwordsL :: [LocalizedString] -> LocalizedString
unwordsL list = mconcat $ intersperse (Untranslated $ T.pack " ") list

