{-# LANGUAGE RecordWildCards #-}

module Data.Gmo where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as BI
import qualified Data.Trie as T
import Data.Word
import Text.Printf
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO.MMap

import Debug.Trace

data GmoFile = GmoFile {
  fMagic :: Word32,
  fRevision :: Word32,
  fSize :: Word32,
  fOriginalOffset :: Word32,
  fTranslationOffset :: Word32,
  fHashtableSize :: Word32,
  fHashtableOffset :: Word32,
  fOriginals :: [(Word32, Word32)],
  fTranslations :: [(Word32, Word32)],
  fData :: L.ByteString
  }
  deriving (Eq)

instance Show GmoFile where
  show f = printf "<GMO file size=%d>" (fSize f)

data GmoData = GmoData {
  gmoSize :: Word32,
  gmoData :: T.Trie [L.ByteString] }
  deriving (Eq)

instance Show GmoData where
  show gmo = printf "<GMO data size=%d>" (gmoSize gmo)

unpackGmoFile :: GmoFile -> GmoData
unpackGmoFile (GmoFile {..}) = GmoData fSize trie
  where
    getBS (len,offs) = L.take (fromIntegral len) $ L.drop (fromIntegral offs) fData
    
    getBSs (len,offs) =
      let bstr = getBS (len,offs)
      in  if L.null bstr
            then [L.empty]
            else L.split 0 bstr

    originals = map L.toStrict $ map getBS fOriginals
    translations = map getBSs fTranslations

    trie = T.fromList $ zip originals translations

parseGmo :: Get GmoFile
parseGmo = do
  magic <- getWord32host
  if magic /= 0x950412de && magic /= 0xde120495
    then fail "Invalid magic number"
    else do
         revision <- getWord32host
         size <- getWord32host
         origOffs <- getWord32host
         transOffs <- getWord32host
         hashSz <- getWord32host
         hashOffs <- getWord32host
         origs <- replicateM (fromIntegral size) getPair
         trans <- replicateM (fromIntegral size) getPair
         return $ GmoFile {
                    fMagic = magic,
                    fRevision = revision,
                    fSize = size,
                    fOriginalOffset = origOffs,
                    fTranslationOffset = transOffs,
                    fHashtableSize = hashSz,
                    fHashtableOffset = hashOffs,
                    fOriginals = origs,
                    fTranslations = trans,
                    fData = undefined }

getPair :: Get (Word32, Word32)
getPair = do
  x <- getWord32host
  y <- getWord32host
  return (x,y)

withGmoFile :: FilePath -> (GmoFile -> IO a) -> IO a
withGmoFile path go = do
  content <- L.readFile path
  let gmo = (runGet parseGmo content) {fData = content}
  print $ L.length $ fData gmo
  result <- go gmo
  return result

