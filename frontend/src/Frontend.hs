{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           Data.Map                       ( Map )

import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Generated.Static

import           Reflex.Dom.Core

import           Common.Api
import           Common.Route


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
                       el "title" $ text "Obelisk Minimal Example"
                       elAttr
                         "link"
                         (  "href"
                         =: static @"main.css"
                         <> "type"
                         =: "text/css"
                         <> "rel"
                         =: "stylesheet"
                         )
                         blank
  , _frontend_body = do
                       el "h1" $ text "Welcome to Obelisk!"
                       el "p" $ text $ T.pack commonStuff

                       -- `prerender` and `prerender_` let you choose a widget to run on the server
                       -- during prerendering and a different widget to run on the client with
                       -- JavaScript.
                       -- On reload, the server-generated SVG is visible for a moment,
                       -- then disappears when the client has rendered.
                       prerender_ simpleSvgWithNamespace simpleSvgWithNamespace

                       -- This works:
                       --simpleSvgWithNamespace

                       return ()
  }

elSvgAttr
  :: (DomBuilder t m, PostBuild t m)
  => Text
  -> Dynamic t (Map Text Text)
  -> m a
  -> m (Element EventResult (DomBuilderSpace m) t, a)
elSvgAttr = elDynAttrNS' $ Just "http://www.w3.org/2000/svg"

simpleSvgWithNamespace :: (DomBuilder t m, PostBuild t m) => m ()
simpleSvgWithNamespace = do
  _ <-
    elSvgAttr
        "svg"
        (constDyn
          (  "xmlns"
          =: "http://www.w3.org/2000/svg"
          <> "width"
          =: "100%"
          <> "height"
          =: "100"
          )
        )
      $ innerPart
  return ()
 where
  innerPart :: (DomBuilder t m, PostBuild t m) => m ()
  innerPart = do
    _ <-
      elSvgAttr
          "rect"
          (constDyn
            ("width" =: "100%" <> "height" =: "100%" <> "fill" =: "green")
          )
        $ blank
    return ()
