-- {-# LANGUAGE OverloadedStrings #-}
--
-- module Main where
--
--   import Web.Scotty
--
--   import Network.HTTP.Types
--   -- import Text.Blaze.Html5
--   import           Text.Blaze.Html5            (Html, a, body, button,
--                                                 dataAttribute, div, docTypeHtml,
--                                                 form, h1, h2, head, input, li,
--                                                 link, meta, p, script, style,
--                                                 title, ul, (!), img)
--   import Text.Blaze.Html5.Attributes
--   import qualified Web.Scotty as S
--   import Network.Wai.Middleware.RequestLogger
--   import Network.Wai.Middleware.Static
--   -- import qualified Text.Blaze.Html5 as H
--   import qualified Text.Blaze.Html5.Attributes as A
--   import Text.Blaze.Html.Renderer.Text
--   import Model
--   import qualified Views.Index
--
--   blaze :: Html -> ActionM ()
--   blaze = html . renderHtml
--
--   main :: IO ()
--   main = do
--     runDb $ runMigration migrateAll
--     scotty 3000 $ do
--       middleware logStdoutDev
--       middleware $ staticPolicy (noDots >-> addBase "static")
--
--       get "/" $ do
--         now <- liftIO getCurrentTime
--         liftIO $ runDb $ insert $ Post _title "some content" now
--         blaze $ do
--           img ! src "/imgs/foo.png"

{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts, TypeSynonymInstances, MultiParamTypeClasses #-}

module Main where
  import Func
  import qualified Web.Scotty as S hiding (body)
  import           CSS                  (layoutCss)
  import           Text.Blaze.Html5            (Html, a, br, body, button, toValue,
                                                dataAttribute, div, docTypeHtml,
                                                form, h1, h2, head, input, li,
                                                link, meta, p, script, style,
                                                title, ul, (!), toHtml, preEscapedText, html)
  import           Text.Blaze.Html5.Attributes (charset, class_, content, href,
                                                httpEquiv, id, media, name,
                                                placeholder, rel, src, type_, accept,
                                                method, action, enctype)
  import Text.Blaze.Html.Renderer.Text
  import Database.Persist
  import Database.Persist.Sqlite
  import Database.Persist.TH
  import Data.Text (Text)
  import Data.Time (UTCTime, getCurrentTime)
  import qualified Data.Text as T
  import Control.Monad.IO.Class (liftIO)
  import Control.Monad.Trans.Resource (runResourceT, ResourceT)
  -- import Database.Persist.GenericSql
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

  -- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
  -- Photo
  --     photoId Int
  --     imgSrc Text
  --     createdAt UTCTime
  --     deriving Show
  --     |]
  --
  -- runDb :: SqlPersist (ResourceT (NoLoggingT IO)) a -> IO a
  -- runDb query =   runNoLoggingT
  --               . runResourceT
  --               . withSqliteConn "dev.app.sqlite3"
  --               . runSqlConn $ query
  --
  -- readPhotos :: IO [Entity Photo]
  -- readPhotos = (runDb $ selectList [] [LimitTo 10])
  --
  -- blaze = S.html . renderHtml
  --
  -- pet = preEscapedText
  --
  -- layout :: Html -> Html -> Html
  -- layout t b = docTypeHtml $ do
  --   -- pet "<!--[if lt IE 7]>      <html class='no-js lt-ie9 lt-ie8 lt-ie7'> <![endif]-->"
  --   -- pet "<!--[if IE 7]>         <html class='no-js lt-ie9 lt-ie8'/> <![endif]-->"
  --   -- pet "<!--[if IE 8]>         <html class='no-js lt-ie9'> <![endif]-->"
  --   -- pet "<!--[if gt IE 8]><!--> <html class='no-js'> <!--<![endif]-->"
  --   head $ do
  --     title t
  --     meta ! charset "utf-8"
  --     meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
  --     meta ! name "description" ! content "Inspire Text"
  --     meta ! name "viewport" ! content "width=device-width"
  --     link ! href "//netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css" ! rel  "stylesheet" ! media "screen"
  --     body $ do
  --       script ! src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" $ mempty
  --       script ! src "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js" $ mempty

  main :: IO ()
  main = S.scotty 3000 $ do
      S.middleware logStdoutDev
      S.middleware $ staticPolicy (noDots >-> addBase "uploads")

      S.get "/" $ do
          S.html $ renderHtml
              $ html $ do
                  body $ do
                      form ! method "post" ! enctype "multipart/form-data" ! action "/upload" $ do
                          input ! type_ "file" ! name "turn down for what"
                          br
                          -- input ! type_ "file" ! name "barfile"
                          br
                          input ! type_ "submit"

      S.post "/upload" $ do
          fs <- S.files
          let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- fs ]
          -- let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi, mconcat [TL.pack $ takeExtension $ BS.unpack $ fileName fi]) | (fieldName,fi) <- fs ]
        -- write the files to disk, so they will be served by the static middleware
          -- liftIO $ sequence_ [ B.writeFile ("uploads" </> fn) fc | (_,fn,fc) <- fs' ]
          -- liftIO $ sequence_ [ B.writeFile (TL.unpack $ mconcat ["uploads", src]) fc | (_,fn,fc,src) <- fs' ]
          liftIO $ sequence_ [ B.writeFile ("uploads" </> fn) fc | (_,fn,fc) <- fs' ]
          liftIO $ sequence_ [makeFilter("uploads" </> fn) | (_,fn,_) <- fs']

          -- liftIO $ sequence_ [
          --                      makeFilter ("frontend/img/" ++ (TL.unpack src)) >>= (\val -> if val
          --                                                                                     then do
          --                                                                                              (TL.pack $ show $ id) (mconcat [TL.pack "/img/", src])
          --                                                                                     else do
          --                                                                                             \(TL.pack $ show $ id))
          --                        | (_,_,_,src) <- fs']

          -- liftIO $ sequence_ [makeFilter("uploads" ++ (TL.unpack src))| (_, _, _, src) <- fs']
          S.html $ mconcat [ mconcat [ fName
                                    , ": "
                                    , renderHtml $ a (toHtml fn) ! (href $ toValue fn) >> br
                                    ]
                                    | (fName,fn,_) <- fs']

        -- generate list of links to the files just uploaded
          -- S.html $ mconcat [ mconcat [ fName
          --                         , ": "
          --                         , renderHtml $ a (toHtml fn) ! (href $ toValue fn) >> br
          --                         ]
          --                         | (fName,fn,_) <- fs' ]

      -- S.get "/create/:photoId" $ do
      --   _photoId <- S.param "photoId"
      --   now <- liftIO getCurrentTime
      --   liftIO $ runDb $ insert $ Photo _photoId "some content" now
      --   S.redirect "/"
      --
      -- S.get "/upload" $ do
      --    blaze $ layout "home" $ do
      --     div ! class_ "container" $ do
      --       div ! class_ "jumbotron" $ do
      --         h1 "Psevdogram"
      --         p "Добро пожаловать. Нажмите кнопку ниже для того, чтобы загрузить изображение и (в дальнейшем) использовать фильтр"
      --         input ! type_ "file" ! name "file" ! accept "image/*,image/jpeg"
      --         p $ do a ! class_ "btn btn-lg btn-primary" ! id "fb" ! href "/upload" $ "Загрузить"
      --
      --
      -- S.post "/upload" $ do
      --   fs <- S.files
      --   let fs' = [ (fieldName, B.unpack (fileName fi), fileContent fi, mconcat [T.pack $ show $ id, T.pack $ takeExtension $ B.unpack $ fileName fi]) | (fieldName,fi) <- fs ]
      --   liftIO $ sequence_ [ B.writeFile (B.unpack $ mconcat ["frontend/img/", src]) fc | (_,fn,fc,src) <- fs' ]
      --
      --
      -- S.get "/" $ do
      --   _photos <- liftIO readPhotos
      --   let photos = map (photoPhotoId . entityVal) _photos
      --   blaze $ do
      --     ul $ do
      --       forM_ photos $ \photo -> li (toHtml photo)
