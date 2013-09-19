
import Data.Gmo

import Control.Monad
import qualified Data.Trie as T
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text.Lazy.IO as TLIO
import System.Environment

main = do
  [file] <- getArgs
  withGmoFile file $ \f -> do
    let gmo = unpackGmoFile f
    print $ gmoSize gmo
    forM_ (T.toList $ gmoData gmo) $ \(orig,trans) -> do
      putStr "Original: "
      B8.putStrLn orig
      forM_ (zip [0..] trans) $ \(i,tran) -> do
        putStr $ "Translation #" ++ show i ++ ": "
        TLIO.putStrLn tran
