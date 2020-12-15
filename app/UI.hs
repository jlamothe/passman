{-

passman
Copyright (C) 2018-2020 Jonathan Lamothe
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

{-# LANGUAGE LambdaCase #-}

module UI (getMasterPass, mainMenu) where

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
      reqIO $ putStrLn "The passwords you entered do not match."
      reqFail
    else return p1

mainMenu :: S.StateT Status IO ()
mainMenu =
  menu "Main Menu"
    [ ( "view/edit a password",   viewEditMenu     )
    , ( "add a password",         addPassword      )
    , ( "change master password", changeMasterPass )
    , ( "save manually",          save >> mainMenu )
    , ( "lock session",           lockSession      )
    , ( "quit",                   quit             )
    ]

addPassword :: S.StateT Status IO ()
addPassword = do
  pass <- S.gets (^.masterPass)

  lift (runRequest $ prompt "confirm master password: " reqPassword)
    >>= \case
      Nothing -> mainMenu
      Just chkPass
        | pass == chkPass -> addPassword'

        | otherwise -> do
          lift $ putStrLn "Incorrect master password."
          mainMenu

addPassword' :: S.StateT Status IO ()
addPassword' = do
  svc <- req $ prompt "service name: " reqResp
  ifServExists svc
    (do
      edit <- req (confirm $
        "The service already exists in the database.\n" ++
        "Would you like to edit it?")
      if edit
        then servMenu svc
        else mainMenu)
    (do
      d <- buildData
      setService svc d
      showPass svc
      servMenu svc)

viewEditMenu :: S.StateT Status IO ()
viewEditMenu = menu "View/Edit Password"
  [ ( "search services", searchServ )
  , ( "list services",   listServ   )
  , ( "cancel",          mainMenu   )
  ]

changeMasterPass :: S.StateT Status IO ()
changeMasterPass = do
  req (confirm $
    "\nWARNING: Changing your master password will change all of your saved passwords.\n" ++
    "Are you sure you would like to proceed?") >>= flip when
    (do
      oldP <- S.gets $ view masterPass
      newP <- req $ reqDefault getMasterPass oldP
      S.modify $ set masterPass newP)
  mainMenu

lockSession :: S.StateT Status IO ()
lockSession = do
  lift $ putStrLn "\nThe session is locked."
  pass <- S.gets $ view masterPass
  x <- req $ prompt "master password: " reqPassword
  if x == pass
    then mainMenu
    else lockSession

quit :: S.StateT Status IO ()
quit = save

buildData :: S.StateT Status IO PWData
buildData = do
  d <- run newPWData
  req $ reqIf (confirm "Would you like to change the password policy?")
    (do
      let p = d^.pwPolicy
      p' <- reqDefault (editPolicy p) p
      return $ set pwPolicy p' d)
    (return d)

searchServ :: S.StateT Status IO ()
searchServ = do
  svc <- req $ prompt "service name: " reqResp
  db  <- S.gets $ view database
  case pwSearch svc db of
    []  -> do
      lift $ putStrLn "\nThe service could not be found in the database."
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
  [ ( "show password",  showPass x >> servMenu x )
  , ( "edit password",  editPassMenu x           )
  , ( "remove service", removeServ x             )
  , ( "rename service", renameServ x             )
  , ( "back",           mainMenu                 )
  ]

editPassMenu :: String -> S.StateT Status IO ()
editPassMenu x = menu (x ++ " : Edit Password")
  [ ( "generate new password", changeSalt x   )
  , ( "edit password policy",  doEditPolicy x )
  , ( "back",                  servMenu x     )
  ]

removeServ :: String -> S.StateT Status IO ()
removeServ x = do
  go <- req $ confirm $
    "Are you sure you want to delete the password for " ++ x ++ "?"
  if go
    then do
      removeServ' x
      mainMenu
    else servMenu x

removeServ' :: String -> S.StateT Status IO ()
removeServ' = S.modify . over database . pwRemoveService

renameServ :: String -> S.StateT Status IO ()
renameServ x = do
  y <- req $ prompt "new service name: " reqResp
  if x == y
    then servMenu x
    else ifServExists y
      (do
        overwrite <- req $ confirm $
          y ++ " already exists.\n" ++
          "Would you like to overwrite it?"
        if overwrite
          then renameServ' x y
          else servMenu x)
      (renameServ' x y)

renameServ' :: String -> String -> S.StateT Status IO ()
renameServ' x y = withService x mainMenu $ \d -> do
  removeServ' x
  setService y d
  servMenu y

changeSalt :: String -> S.StateT Status IO ()
changeSalt x = withService x mainMenu $ \d -> do
  salt <- run newPWSalt
  setService x $ set pwSalt salt d
  showPass x
  editPassMenu x

doEditPolicy :: String -> S.StateT Status IO ()
doEditPolicy x = withService x mainMenu $ \d -> do
  let p = d^.pwPolicy
  p' <- req $ reqDefault (editPolicy p) p
  setService x $ set pwPolicy p' d
  showPass x
  editPassMenu x

showPass :: String -> S.StateT Status IO ()
showPass x = do
  lift $ putStrLn ""
  withService x
    (lift $ putStrLn "The service could not be found in the database.") $
    \d -> do
      mp <- S.gets $ view masterPass
      lift $ putStrLn $ case pwGenerate mp d of
        Nothing -> "The password data were not valid."
        Just pw -> "password for " ++ x ++ ": " ++ pw

-- TODO: refactor this monstrosity
editPolicy :: PWPolicy -> Request PWPolicy
editPolicy policy = do
  p <-
    edit "length" (policy^.pwLength) pwLength policy >>=
    edit "min upper case"  (policy^.pwUpper) pwUpper >>=
    edit "min lower case" (policy^.pwLower) pwLower  >>=
    edit "min digits" (policy^.pwDigits) pwDigits    >>=
    special
  if validatePWPolicy p
    then return p
    else do
      reqIO $ putStrLn $
        "\nThe password policy you entered is invalid\n." ++
        "It will not be changed."
      reqFail
  where
    edit l v t p = do
      v' <- reqDefault
        (prompt ("new " ++ l ++ " (default " ++ show v ++ "): ") reqInt) v
      return $ set t v' p
    special p = do
      reqIO $ putStrLn $ "Special characters are currently " ++
        (case p^.pwSpecial of
          Nothing -> "not "
          Just _  -> "") ++ "allowed."
      reqIf (confirm "Would you like to allow special characters?")
        (case p^.pwSpecial of
          Nothing -> do
            x <- required $ prompt "min special chars: " reqInt
            return $ set pwSpecial (Just x) p
          Just x -> edit "min special chars" x (pwSpecial.traverse) p)
        (return $ set pwSpecial Nothing p)

--jl
