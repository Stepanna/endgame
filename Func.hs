{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Func where

import Prelude hiding (catch)
import qualified Web.Scotty as S hiding (body)
import           Text.Blaze.Html5            (Html, a, br, body, button, toValue,
                                              dataAttribute, div, docTypeHtml,
                                              form, h1, h2, head, input, output,
                                              link, meta, p, script, style, li,
                                              title, ul, (!), toHtml, preEscapedText, html, img)
import           Text.Blaze.Html5.Attributes (charset, class_, content, href,
                                              httpEquiv, id, media, name, value,
                                              placeholder, rel, src, type_, accept,
                                              method, action, enctype)
import Text.Blaze.Html.Renderer.Text
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
                           (savePngImage path  .M.ImageRGB8 . rotateImg) (convertRGB8 img)
                           print "OK FILTERED!!!"

                           return True

rotateImg :: M.Image M.PixelRGB8 -> M.Image M.PixelRGB8
rotateImg img@M.Image {..} = runST $ do
    mimg <- M.newMutableImage imageWidth imageHeight
    let go x y
           | x >= imageWidth  = go 0 (y + 1)
           | y >= imageHeight = M.unsafeFreezeImage mimg  --unsafe convert a mutable image to an immutable
           | otherwise = do
               writePixel mimg x y  (pixelAt img x y)
               go (x + 1) y
    go 0 0


makeFilter2 :: String -> IO(Bool)
makeFilter2 path = do
    eimg <- readImage path
    case eimg of
           Left err -> do
                          print $ "Err" ++ path
                          removeIfExists path
                          return False
           Right img -> do
                           (savePngImage path . M.ImageRGB8 . brightnessRGB8 30) (convertRGB8 img)
                           print "OK FILTERED!!!"
                           return True


brightnessRGB8 :: Int -> M.Image M.PixelRGB8 -> M.Image M.PixelRGB8
brightnessRGB8 add = M.pixelMap brightFunction
     where up v = fromIntegral (fromIntegral v + add)
           brightFunction (PixelRGB8 r g b) =
                   PixelRGB8 (up r) (up g) (up b)

makeFilter3 :: String -> IO(Bool)
makeFilter3 path = do
    eimg <- readImage path
    case eimg of
           Left err -> do
                          print $ "Err" ++ path
                          removeIfExists path
                          return False
           Right img -> do
                           (savePngImage path . M.ImageRGB8 . negativRGB8) (convertRGB8 img)
                           print "OK FILTERED!!!"
                           return True

negativRGB8 :: M.Image M.PixelRGB8 -> M.Image M.PixelRGB8
negativRGB8 = M.pixelMap negativFunction
     where negativFunction (PixelRGB8 r g b) =
                   PixelRGB8 (255 - r) (255 - g) (255 - b)


removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
