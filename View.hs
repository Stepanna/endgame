{-# LANGUAGE OverloadedStrings #-}

module View (homeView, blaze, layout, navBar) where

  import           CSS                  (layoutCss)
  import           Data.Monoid                 (mempty)
  import           Data.Text.Lazy              (toStrict)
  import           Prelude                     hiding (div, head, id)
  import           Text.Blaze.Html             (Html, toHtml)
  import           Text.Blaze.Html5            (Html, a, body, button,
                                                dataAttribute, div, docTypeHtml,
                                                form, h1, h2, head, input, li,
                                                link, meta, p, script, style, br,
                                                title, ul, (!))
  import           Text.Blaze.Html5.Attributes (charset, class_, content, href,
                                                httpEquiv, id, media, name,
                                                placeholder, rel, src, type_, accept,
                                                method, action, enctype)
  -- import           Views.Utils                 (blaze, pet)
  import           Web.Scotty                  (ActionM, html)
  import           Text.Blaze.Html.Renderer.Text (renderHtml)
  import           Text.Blaze.Internal           (preEscapedText)

  blaze :: Html -> ActionM ()
  blaze = html . renderHtml

  pet = preEscapedText

  layout :: Html -> Html -> Html
  layout t b = docTypeHtml $ do
    -- pet "<!--[if lt IE 7]>      <html class='no-js lt-ie9 lt-ie8 lt-ie7'> <![endif]-->"
    -- pet "<!--[if IE 7]>         <html class='no-js lt-ie9 lt-ie8'/> <![endif]-->"
    -- pet "<!--[if IE 8]>         <html class='no-js lt-ie9'> <![endif]-->"
    -- pet "<!--[if gt IE 8]><!--> <html class='no-js'> <!--<![endif]-->"
    head $ do
      title t
      meta ! charset "utf-8"
      meta ! httpEquiv "X-UA-Compatible" ! content "IE=edge,chrome=1"
      meta ! name "description" ! content "Inspire Text"
      meta ! name "viewport" ! content "width=device-width"
      link ! href "//netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css" ! rel  "stylesheet" ! media "screen"
      style $ pet $ toStrict layoutCss
      body $ do
        navBar >> b
        script ! src "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js" $ mempty
        script ! src "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js" $ mempty

  homeView :: ActionM ()
  homeView = blaze $ layout "home" $ do
    div ! class_ "container" $ do
      div ! class_ "jumbotron" $ do
        p "Rotation"
        form ! method "post" ! enctype "multipart/form-data" ! action "/rotate" $ do
            input ! class_ "btn btn-lg btn-primary" ! type_ "file" ! name "turn down for what"
            br
            input ! class_ "btn btn-lg btn-primary" ! type_ "submit"
            br
        br
        p "Brightness"
        form ! method "post" ! enctype "multipart/form-data" ! action "/bright" $ do
            input ! class_ "btn btn-lg btn-primary" ! type_ "file" ! name "brightness"
            br
            input ! class_ "btn btn-lg btn-primary" ! type_ "submit"
            br
        br
        p "Negative"
        form ! method "post" ! enctype "multipart/form-data" ! action "/negativ" $ do
            input ! class_ "btn btn-lg btn-primary" ! type_ "file" ! name "negativ"
            br
            input ! class_ "btn btn-lg btn-primary" ! type_ "submit"
            br

  navBar :: Html
  navBar = div ! class_ "navbar navbar-default navbar-static-top" $ div ! class_ "container" $ do
    div ! class_ "navbar-header" $ do
      button ! type_ "button"
             ! class_ "navbar-toggle" ! dataAttribute "toggle" "collapse" ! dataAttribute "target" ".navbar-collapse" $ do
        a ! class_ "navbar-brand" ! href "/" $ "λ"
    div ! class_ "navbar-collapse collapse" $ ul ! class_ "nav navbar-nav" $ do
          li ! class_ "active" $ a ! href "/" $ "Home"
