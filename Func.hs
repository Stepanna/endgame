{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Func where

import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)

import Codec.Picture
import Control.Monad.ST
import qualified Codec.Picture.Types as M

makeFilter :: String -> IO(Bool)
makeFilter path = do
    eimg <- readImage path
    case eimg of
           Left err -> do
                          print $ "ERROR!!!!" ++ path
                          removeIfExists path
                          return False
           Right img -> do
                           (savePngImage path . M.ImageRGB8 . rotateImg) (convertRGB8 img)
                           print "OK FILTERED!!!"s
                           return True

rotateImg :: M.Image M.PixelRGB8 -> M.Image M.PixelRGB8
rotateImg img@M.Image {..} = runST $ do
    mimg <- M.newMutableImage imageWidth imageHeight
    let go x y
           | x >= imageWidth  = go 0 (y + 1)
           | y >= imageHeight = M.unsafeFreezeImage mimg  --unsafe convert a mutable image to an immutable
           | otherwise = do
               writePixel mimg
                  (imageWidth - x - 1)
                  (imageHeight - y - 1)
                  (pixelAt img x y)
               go (x + 1) y
    go 0 0

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
