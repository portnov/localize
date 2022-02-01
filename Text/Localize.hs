{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, DeriveDataTypeable #-}
-- | This is the main module of the @localize@ package.
-- It contains definitions of general use and reexport generally required internal modules.
module Text.Localize
  (
   -- $description
   --
   -- * Most used functions
   __, __n, __f,
   -- * Basic functions
   translate, translateN, translateNFormat, 
   lookup, withTranslation,
   -- * Reexports
   module Text.Localize.Types,
   module Text.Localize.Load,
   module Text.Localize.Locale
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
import Text.Localize.Locale

-- $description
--
-- This is the main module of the @localize@ package. In most cases, you have to import only
-- this module. In specific cases, you may also want to import Text.Localize.IO or Text.Localize.State.
--
-- All functions exported by @localize@ package work with any instance of the
-- @Localized@ type class. There are two simple examples of this type class
-- implementation provided in separate modules (IO and State); however, in
-- complex applications it may be more convinient to implement @Localized@
-- instance for the monadic stack you already have.
--
-- Example of usage is:
--
-- @
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy.IO as TLIO
-- import Text.Localize
-- 
-- newtype MyMonad a = MyMonad {unMyMonad :: ... }
--   deriving (Monad)
-- 
-- instance Localized MyMonad where
--   ...
--
-- runMyMonad :: Translations -> MyMonad a -> IO a
-- runMyMonad = ...
-- 
-- hello :: T.Text -> MyMonad ()
-- hello name = do
--   liftIO $ TLIO.putStrLn =<< __ "Your name: "
--   liftIO $ hFlush stdout
--   name <- liftIO $ TLIO.getLine
--   greeting <- __f "Hello, {}!" (Single name)
--   liftIO $ TLIO.putStrLn greeting
--
-- main :: IO ()
-- main = do
--   translations <- locateTranslations $ linuxLocation "hello"
--   runMyMonad translations hello
--
-- @
--
-- See also working examples under @examples/@ directory.
--

-- | Look up for translation. Returns source string if translation not found.
lookup :: Translations -> LanguageId -> TranslationSource -> T.Text
lookup t lang bstr =
  case M.lookup lang (tMap t) of
    Nothing -> toText bstr
    Just gmo -> Gettext.gettext gmo bstr

-- | Execute function depending on current translation catalog and context.
withTranslation :: Localized m
                => (b -> r)    -- ^ Function to be executed if there is no
                               --   translation for current language loaded.
                -> (Gettext.Catalog -> Maybe Context -> b -> r) -- ^ Function to be executed on current catalog.
                -> (b -> m r)  -- ^ Function lifted into @Localized@ monad.
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
translateFormat :: (Localized m, MonadFail m, F.VarContainer vars)
                => TranslationSource -- ^ Original formatting string
                -> vars              -- ^ Substitution variables
                -> m T.Text
translateFormat orig vars = do
  fmtStr <- translate orig
  case parseFormat fmtStr of
    Left err -> fail $ show err
    Right fmt -> return $ F.format fmt vars

-- | Short alias for @translateFormat@.
__f ::  (Localized m, MonadFail m, F.VarContainer c) => TranslationSource -> c -> m T.Text
__f = translateFormat

-- | Translate a string, taking plural forms into account,
-- and substitute variables into it.
-- Data.Text.Format.Heavy.format syntax is used.
translateNFormat :: (Localized m, MonadFail m, F.VarContainer vars)
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
__n :: (Localized m, MonadFail m, F.VarContainer c) => TranslationSource -> TranslationSource -> Int -> c -> m T.Text
__n = translateNFormat

