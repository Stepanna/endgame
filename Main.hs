{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}


module Main where
  import Func
  import View (layout, homeView, blaze, navBar)
  import qualified Web.Scotty as S hiding (body)
  import           CSS                  (layoutCss)
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
  import Database.Persist
  import Database.Persist.Sqlite
  import Database.Persist.TH
  import Data.Text (Text, pack)
  import Data.Time (UTCTime, getCurrentTime)
  import qualified Data.Text as T
  import Control.Monad.IO.Class (liftIO)
  import Control.Monad.Trans.Resource (runResourceT, ResourceT)
  import Control.Monad (forM_)
  import Control.Applicative
  import Control.Monad.Logger
  import Data.ByteString.Lazy as B hiding (map, head)
  import Prelude hiding (div, head, id)
  import Network.Wai.Middleware.RequestLogger
  import Network.Wai.Middleware.Static
  import Network.Wai.Parse
  import Control.Monad.IO.Class
  import qualified Data.ByteString.Char8 as BS
  import qualified Data.Text.Lazy as TL
  import Data.Monoid
  import System.FilePath ((</>))
  import System.FilePath.Posix hiding ((</>))
  import GHC.Exts(fromString)


  main :: IO ()
  main = S.scotty 3000 $ do
      S.middleware logStdoutDev
      S.middleware $ staticPolicy (noDots >-> addBase "uploads")

      S.get "/" $ homeView

      S.post "/rotate" $ do
          fs <- S.files
          let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- fs ]

          liftIO $ sequence_ [B.writeFile ("uploads" </> fn) fc | (_,fn,fc) <- fs' ]
          liftIO $ sequence_ [makeFilter("uploads" </> fn) | (_,fn,_) <- fs']
          let ref = toValue $ mconcat [  fn | (fName,fn,_) <- fs']
          S.html $ do
            renderHtml $ layout "rotate"  do
            div ! class_ "container" $ do
              div ! class_ "jumbotron" $ do
                h2 $ a ! href ref $ "result"



      S.post "/bright" $ do
          fs <- S.files
          let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- fs ]

          liftIO $ sequence_ [B.writeFile ("uploads" </> fn) fc | (_,fn,fc) <- fs' ]
          liftIO $ sequence_ [makeFilter2("uploads" </> fn) | (_,fn,_) <- fs']
          let ref = toValue $ mconcat [  fn | (fName,fn,_) <- fs']

          S.html $ do
            renderHtml $ layout "bright"  do
            div ! class_ "container" $ do
              div ! class_ "jumbotron" $ do
                h2 $ a ! href ref $ "result"

      S.post "/negativ" $ do
        fs <- S.files
        let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- fs ]

        liftIO $ sequence_ [B.writeFile ("uploads" </> fn) fc | (_,fn,fc) <- fs' ]
        liftIO $ sequence_ [makeFilter3("uploads" </> fn) | (_,fn,_) <- fs']
        let ref = toValue $ mconcat [  fn | (fName,fn,_) <- fs']
        S.html $ do
          renderHtml $ layout "negativ"  do
          div ! class_ "container" $ do
            div ! class_ "jumbotron" $ do
              h2 $ a ! href ref $ "result"
