{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module Templates.Index where

import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as A
import Data.Time
import System.Locale

import Config
import Types

type Username       = String
type Arty5          = Bool
type CurrentStatus  = String


--------------------------------------------------------------------------------
-- The main document

documentT :: Html
documentT = H.docTypeHtml $ do

    H.head $ do

        H.meta ! A.charset "utf-8"
        H.title "whoLogin v3.0"
        H.link ! A.rel "stylesheet" ! A.href "/css/style.css" ! A.type_ "text/css"
        H.script ! A.src "/js/jquery-1.4.4.min.js" $ return ()
        H.script ! A.src "/js/whologin.js"         $ return ()
        H.script ! A.src "/js/main.js"             $ return ()

    H.body $ do

        H.h1 ! A.id "app-title" $ "whoLogin v3.0"

        H.div ! A.id "ajax-status" $ do
            H.img ! A.src "/img/loading.gif"
            p "Loading ..."

        H.menu $ H.ul $ mapM_ (H.li ! A.class_ "menu-item")
            [ H.a ! href "#" ! A.id "menu-status"   $ "Status"
            , H.a ! href "#" ! A.id "menu-settings" $ "Settings"
            ]

        H.section ! A.id "main" $ do

            return ()


--------------------------------------------------------------------------------
-- Status templates

statusT :: CurrentStatus
        -> Log
        -> Html
statusT cur l = do

    H.h1 $ do "Current Status:"
              H.span ! A.id "status-cur" $ H.string cur

    H.p "History:"
    H.pre ! A.id "status-history" $ H.string $ unlines l'

  where
    l' = map (\(c,msg) -> formatTime defaultTimeLocale "%R" c ++ " - " ++ msg) l


--------------------------------------------------------------------------------
-- Settings templates

settingsT :: Config -> Html
settingsT Config { login, passArty5, interval } = do

    if null login then
        H.h1 "Settings"
      else
        H.h1 . H.string $ "Settings for " ++ login

    -- Hidden by default:
    H.p ! A.id "settings-updated" ! A.class_ "success" $ "Settings updated."

    H.form ! A.method "post" ! A.action "/settings/post" ! A.id "settings-form" $ do

        H.ul ! A.id "settings-user" $ mapM_ H.li

            [ do H.label "Username:"
                 H.input ! A.name "username" ! A.type_ "text"     ! A.value (H.stringValue login)
                 H.p     ! A.class_ "form-error" ! A.id "username-err" $ return ()  -- placeholders for error reports

            , do H.label "Password:"
                 H.input ! A.name "password" ! A.type_ "password" ! A.value ""
                 H.p     ! A.class_ "form-error" ! A.id "password-err" $ return ()

            , do H.label "Time interval (seconds):"
                 H.input ! A.name "interval" ! A.type_ "text"     ! A.value (H.stringValue $ show interval)
                 H.p     ! A.class_ "form-error" ! A.id "interval-err" $ return ()

            , do H.label "Pass Arty5 website:"
                 H.input ! A.name "arty5"    ! A.type_ "checkbox" ! A.checked (if passArty5 then "checked" else "")

            ]

        H.div ! A.id "settings-advanced" $ do
            H.p ! A.class_ "ajax-link" $
                H.a ! A.href "#" $ "Advanced settings"
            H.div ! A.id "settings-advanced-content" $ return () -- loaded via ajax

        H.input ! A.id "submit-settings" ! A.type_ "submit" ! A.value "Save settings"

settingsAdvT :: Config -> Html
settingsAdvT Config { bind, port } = do

    H.h2 "Server settings"

    H.p ! A.class_ "important" $
        "These settings shouldn't be changed at all."

    H.p ! A.class_ "important" $
        "Changing these settings requires a restart of the server."

    H.ul ! A.id "settings-server" $ mapM_ H.li

        [ do H.label "Bind to address:"
             H.input ! A.name "bind" ! A.type_ "text" ! A.value (H.stringValue bind)
             H.p     ! A.class_ "form-error" ! A.id "bind-err" $ return ()

        , do H.label "Use port:"
             H.input ! A.name "port" ! A.type_ "text" ! A.value (H.stringValue $ show port)
             H.p     ! A.class_ "form-error" ! A.id "port-err" $ return ()
        ]
