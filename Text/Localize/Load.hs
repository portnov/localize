{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, DeriveDataTypeable, OverloadedStrings, RecordWildCards #-}
-- | This module contains definitions for loading translation catalogs.
module Text.Localize.Load
  ( -- * Data types
    LocatePolicy (..), Facet,
    -- * Main functions
    loadTranslations, locateTranslations,
    -- * Commonly used location policies
    linuxLocation, localLocation
  ) where

import Control.Monad
import Control.Monad.Trans
import Data.Default
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.Gettext as Gettext
import Data.Text.Format.Heavy
import System.Directory
import System.FilePath
import System.FilePath.Glob

import Text.Localize.Types

-- | Load translations when path to each translation file is known.
loadTranslations :: [(LanguageId, FilePath)] -> IO Translations
loadTranslations pairs = do
  res <- forM pairs $ \(lang, path) -> do
           gmo <- Gettext.loadCatalog path
           return (lang, gmo)
  return $ Translations $ M.fromList res

-- | Locale facet (@LC_MESSAGES@ and siblings).
type Facet = String

-- | This data type defines where to search for catalog files (@.mo@ or @.gmo@) in the file system.
data LocatePolicy = LocatePolicy {
    lcBasePaths :: [FilePath] -- ^ Paths to directory with translations, e.g. @"\/usr\/share\/locale"@. Defaults to @"locale"@.
  , lcName :: String       -- ^ Catalog file name (in gettext this is also known as text domain). Defaults to @"messages"@.
  , lcFacet :: Facet       -- ^ Locale facet. Defaults to @LC_MESSAGES@.
  , lcFormat :: Format     -- ^ File path format. The following variables can be used:
                           -- 
                           -- * @{base}@ - path to directory with translations;
                           -- * @{language}@ - language code;
                           -- * @{facet}@ - locale facet;
                           -- * @{name}@ - file name (text domain), without extension.
                           --
                           -- Please note: assumption is made that the @{language}@ variable is used only once.
                           --
                           --  Defaults to @"{base}\/{language}\/{facet}\/{name}.mo"@.
  }
  deriving (Show)

instance Default LocatePolicy where
  def = LocatePolicy {
          lcBasePaths = ["locale"],
          lcName = "messages",
          lcFacet = "LC_MESSAGES",
          lcFormat = "{base}/{language}/{facet}/{name}.mo"
        }

-- | Usual Linux translations location policy.
-- Catalog files are found under @\/usr\/[local\/]share\/locale\/{language}\/LC_MESSAGES\/{name}.mo@.
linuxLocation :: String        -- ^ Catalog file name (text domain)
              -> LocatePolicy
linuxLocation name = def {lcBasePaths = ["/usr/share/locale", "/usr/local/share/locale"], lcName = name}

-- | Simple translations location polciy, assuming all catalog files located at
-- @{base}\/{language}.mo@.
localLocation :: FilePath      -- ^ Path to directory with translations
              -> LocatePolicy
localLocation base = def {lcBasePaths = [base], lcFormat = "{base}/{language}.mo"}

-- | Locate and load translations according to specified policy.
locateTranslations :: MonadIO m => LocatePolicy -> m Translations
locateTranslations (LocatePolicy {..}) = liftIO $ do
    basePaths <- mapM makeAbsolute lcBasePaths
    pairs <- forM basePaths $ \basePath -> do
        let vars = M.fromList $
                     [("base", basePath),
                      ("language", "*"),
                      ("facet", lcFacet),
                      ("name", lcName)] :: M.Map T.Text String
            Format fmtItems = lcFormat
            (fmtBase, fmtTail) = breakFormat fmtItems
            pathGlob = T.unpack (format lcFormat vars)
            pathBaseLen = fromIntegral $ T.length (format (Format fmtBase) vars)
            pathTailLen = fromIntegral $ T.length (format (Format fmtTail) vars)
        paths <- glob pathGlob
        forM paths $ \path -> do
             let pathWithoutBase = drop pathBaseLen path
                 languageLen = length pathWithoutBase - pathTailLen
                 language = take languageLen pathWithoutBase
             return (language, path)
    loadTranslations $ concat pairs
  where
    breakFormat items =
      let (hd, tl) = break isLanguage items
      in  case tl of
            [] -> (hd, [])
            _  -> (hd, tail tl)

    isLanguage (FVariable name _) = name == "language"
    isLanguage _ = False

