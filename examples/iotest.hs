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
import Text.Localize.IO

hello :: T.Text -> IO ()
hello name = do
  tran <- __f "Hello, {}!" (Single name)
  TLIO.putStrLn tran

main :: IO ()
main = do
  liftIO $ putStr "Your name: "
  liftIO $ hFlush stdout
  name <- liftIO $ TLIO.getLine

  setupTranslations $ localLocation "mo"
  
  withLanguage "en" $ hello name
  withLanguage "ru" $ hello name
  withLanguage "fr" $ hello name

