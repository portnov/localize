{-# LANGUAGE TypeSynonymInstances, GeneralizedNewtypeDeriving, OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import System.FilePath
import System.FilePath.Glob
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TLIO
import System.IO
import Data.Text.Format.Heavy (Single (..))

import Text.Localize

enumLanguages :: IO [(LanguageId, FilePath)]
enumLanguages = do
  files <- glob "mo/*.mo"
  let langs = map (\f -> take (length f - 3) f) $ map takeFileName files
  return $ zip langs files

data LocState = LocState {
  lsTranslations :: Translations, 
  lsLanguage :: LanguageId }
  deriving (Show)

newtype Loc a = Loc { unLoc :: StateT LocState IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

setLang :: LanguageId -> Loc ()
setLang lang = Loc $ modify $ \st -> st {lsLanguage = lang}

instance Localized Loc where
  getTranslations = Loc $ gets lsTranslations
  getLanguage = Loc $ gets lsLanguage

runLoc :: Loc a -> IO a
runLoc (Loc loc) = do
  pairs <- enumLanguages
  trans <- loadTranslations pairs
  let emptyState = LocState trans ""
  evalStateT loc emptyState

hello :: T.Text -> Loc ()
hello name = do
  tran <- __f "Hello, {}!" (Single name)
  Loc $ lift $ TLIO.putStrLn tran

main = runLoc $ do
  liftIO $ putStr "Your name: "
  liftIO $ hFlush stdout
  name <- liftIO $ TLIO.getLine
  hello name
  setLang "ru"
  hello name
  setLang "fr"
  hello name
  
