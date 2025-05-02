{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Handler.WidgetsTest where

import Import


getWidgetsTestR :: Handler Html
getWidgetsTestR = defaultLayout $ do
    setTitle "Widget Page"
    toWidget [lucius| h1 { color: green; } |]
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
    toWidget
        [julius|
            $(function() {
                $("h1").click(function(){
                    alert("You clicked on the heading!");
                });
            });
        |]
    toWidgetHead
        [hamlet|
            <meta name=keywords content="some sample keywords">
        |]
    toWidget
        [hamlet|
            <h1>Here's one way of including content
        |]
    [whamlet|<h2>Here's another |]
    toWidgetBody
        [julius|
            alert("This is included in the body itself");
        |]
    toWidgetBody
        [hamlet|<script src=/included-in-body.js>|]
    toWidgetHead
        [hamlet|<script src=/included-in-head.js>|]
