{-

passman
Copyright (C) 2018 Jonathan Lamothe
<jlamothe1980@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this program.  If not, see
<https://www.gnu.org/licenses/>.

-}

module UI (getMasterPass, mainMenu) where

import Control.Applicative ((<|>))
import Control.Lens (over, set, view, (^.))
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S
import System.Console.HCL
  ( Request
  , prompt
  , reqDefault
  , reqFail
  , reqIf
  , reqInt
  , reqIO
  , reqPassword
  , reqResp
  , required
  , runRequest
  )

import Password

import Types
import Util

getMasterPass :: Request String
getMasterPass = do
  p1 <- required $ prompt "master password: " reqPassword
  p2 <- required $ prompt "confirm master password: " reqPassword
  if p1 /= p2
    then do
      reqIO $ putStrLn "passwords do not match"
      reqFail
    else return p1

mainMenu :: S.StateT Status IO ()
mainMenu =
  menu "Main Menu"
    [ ( "add a password",         addPassword      )
    , ( "view/edit a password",   viewEditMenu     )
    , ( "change master password", changeMasterPass )
    , ( "lock session",           lockSession      )
    , ( "quit",                   quit             )
    ]

addPassword :: S.StateT Status IO ()
addPassword = do
  svc <- req $ prompt "service name: " reqResp
  db <- S.gets $ view database
  if pwHasService svc db
    then req (confirm "service exists - overwrite?") >>= flip when (addPassword' svc)
    else addPassword' svc
  mainMenu

addPassword' :: String -> S.StateT Status IO ()
addPassword' x = do
  d <- buildData
  S.modify $ over database $ pwSetService x d
  showPass x

viewEditMenu :: S.StateT Status IO ()
viewEditMenu = menu "View/Edit Password"
  [ ( "search services", searchServ )
  , ( "list services",   listServ   )
  , ( "cancel",          mainMenu   )
  ]

changeMasterPass :: S.StateT Status IO ()
changeMasterPass = do
  oldP <- S.gets $ view masterPass
  newP <- req $ reqDefault getMasterPass oldP
  S.modify $ set masterPass newP
  mainMenu

lockSession :: S.StateT Status IO ()
lockSession = do
  lift $ putStrLn "\nsession locked"
  pass <- S.gets $ view masterPass
  mx <- lift $ runRequest $ prompt "password: " reqPassword
  case mx of
    Nothing -> lockSession
    Just x  -> if x == pass
      then mainMenu
      else lockSession

quit :: S.StateT Status IO ()
quit = return ()

buildData :: S.StateT Status IO PWData
buildData = do
  d <- run newPWData
  req $ reqIf (confirm "would you like to change the default policy?")
    (do
      let p = d^.pwPolicy
      p <- editPolicy p <|> do
        reqIO $ putStrLn "invalid password policy - using default"
        return p
      return $ set pwPolicy p d)
    (return d)

searchServ :: S.StateT Status IO ()
searchServ = do
  svc <- req $ prompt "service name: " reqResp
  db  <- S.gets $ view database
  case pwSearch svc db of
    []  -> do
      lift $ putStrLn "\nservice not found"
      mainMenu
    [x] -> servMenu x
    xs  -> selectServ xs

listServ :: S.StateT Status IO ()
listServ = S.gets (view database) >>= selectServ . pwSearch ""

selectServ :: [String] -> S.StateT Status IO ()
selectServ xs = menu "Select Service" $
  map (\x -> (x, servMenu x)) xs ++
  [("(cancel)", mainMenu)]

servMenu :: String -> S.StateT Status IO ()
servMenu x = menu x
  [ ( "show password", showPass x >> servMenu x )
  , ( "edit password", editPassMenu x           )
  , ( "cancel",        mainMenu                 )
  ]

editPassMenu :: String -> S.StateT Status IO ()
editPassMenu x = menu (x ++ " : Edit Password")
  [ ( "generate new password", changeSalt x )
  , ( "cancel",                servMenu x   )
  ]

changeSalt :: String -> S.StateT Status IO ()
changeSalt x = do
  db <- S.gets $ view database
  case pwGetService x db of
    Nothing   -> mainMenu
    Just serv -> do
      salt <- run newPWSalt
      let serv' = set pwSalt salt serv
      S.modify $ over database (pwSetService x serv')
      showPass x
      servMenu x

showPass :: String -> S.StateT Status IO ()
showPass x = do
  lift $ putStrLn ""
  db <- S.gets $ view database
  case pwGetService x db of
    Nothing -> lift $ putStrLn "service not found"
    Just d  -> do
      pw <- S.gets $ view masterPass
      lift $ putStrLn $ case pwGenerate pw d of
        Nothing -> "invalid password data"
        Just pw -> "password for " ++ x ++ ": " ++ pw

-- TODO: refactor this monstrosity
editPolicy :: PWPolicy -> Request PWPolicy
editPolicy p = if validatePWPolicy p
  then do
    p <- edit "length" (p^.pwLength) pwLength p
    p <- edit "min upper case"  (p^.pwUpper) pwUpper p
    p <- edit "min lower case" (p^.pwLower) pwLower p
    p <- edit "min digits" (p^.pwDigits) pwDigits p
    p <- special p
    if validatePWPolicy p
      then return p
      else reqFail
  else reqFail
  where
    edit l v t p = do
      v <- reqDefault
        (prompt ("new " ++ l ++ " (default " ++ show v ++ "): ") reqInt) v
      return $ set t v p
    special p = do
      reqIO $ putStrLn $ "special chars are currently " ++
        (case p^.pwSpecial of
          Nothing -> "not "
          Just _  -> "") ++ "allowed"
      reqIf (confirm "allow special chars?")
        (case p^.pwSpecial of
          Nothing -> do
            x <- required $ prompt "min special chars: " reqInt
            return $ set pwSpecial (Just x) p
          Just x -> edit "min special chars" x (pwSpecial.traverse) p)
        (return $ set pwSpecial Nothing p)

--jl
