{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend where

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Data.Text (Text)

import Common.Route
import Frontend.NoGardenOnline (app)


htmlTitle :: Text
htmlTitle = "NoGardenOnline"

frontend :: Frontend (R FrontendRoute)
frontend =
    Frontend { _frontend_head = do
                 el "title" $ text htmlTitle
                 elAttr "link" ("href" =: $(static "main.css") <> "rel" =: "stylesheet") blank
             , _frontend_body = app
             }
