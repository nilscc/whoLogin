module WhoAuth ( runWhoAuth
               , Connection (..)
               ) where

import Network.Curl
import Data.List ( isInfixOf )

-- {{{ Network settings

url         = "https://stuwe.net.uni-tuebingen.de"
connUrl     = url ++ "/netaccess/connstatus.html"
formAction  = url ++ "/netaccess/loginuser.html"
formName    = "username"
formPw      = "password"
formSubmit  = "Login"

-- }}}


data Connection = Connected
                | LoginDone
                | Disconnected
                | Rejected

type Passwd = String
type Login = String


runWhoAuth :: Login -> Passwd -> IO Connection

runWhoAuth login pw = do

    let isLoggedIn  = isInfixOf "You are logged in"
        gotRejected = isInfixOf "Credentials Rejected"

    (_, status) <- curlGetString connUrl []

    if isLoggedIn status

       -- already logged in
       then return Connected

       -- try to log in
       else do -- get login page to prevent "SessionExpired" message
               _ <- curlGetString formAction [] 

               -- post user data
               (_, body) <- curlGetString formAction curlOpts 
               case body of
                    l | isLoggedIn l  -> return LoginDone
                      | gotRejected l -> return Rejected
                    _                 -> return Disconnected

  where formPost = [ formName   ++ "=" ++ login
                   , formPw     ++ "=" ++ pw
                   , formSubmit ++ "=Continue"
                   ]
        curlOpts = [ CurlURL formAction
                   , CurlPostFields formPost
                   , CurlFailOnError False
                   ]
