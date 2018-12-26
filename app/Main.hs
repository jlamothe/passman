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

{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative ((<|>))
import Control.Lens (makeLenses, over, set, view, (^.))
import qualified Control.Monad.Trans.State as S
import Control.Monad (join, when)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (mapM_)
import Data.Maybe (fromJust)
import System.Console.HCL
  ( Request (..)
  , prompt
  , reqAgree
  , reqDefault
  , reqFail
  , reqIf
  , reqInt
  , reqIO
  , reqMenu
  , reqPassword
  , reqResp
  , required
  , runRequest
  )
import System.Random (RandomGen (..), StdGen, getStdGen)

import Password

data Status = Status
  { _gen        :: StdGen
  , _masterPass :: String
  , _database   :: PWDatabase
  }

makeLenses ''Status

instance RandomGen Status where
  next s = (x, s') where
    (x, g') = next g
    s' = set gen g' s
    g = s^.gen
  split s = (s1, s2) where
    s1 = set gen g1 s
    s2 = set gen g2 s
    (g1, g2) = split g
    g = s^.gen

main :: IO ()
main = runRequest setup >>= mapM_ (S.evalStateT mainMenu)

setup :: Request Status
setup = do
  g <- reqIO getStdGen
  mp <- getMasterPass
  return $ Status g mp newPWDatabase

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
    , ( "view/edit a password",   viewEditPass     )
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

viewEditPass :: S.StateT Status IO ()
viewEditPass = menu "View/Edit Password"
  [ ( "search services", searchServ )
  , ( "list services",   listServ   )
  , ( "cancel",          mainMenu   )
  ]

searchServ :: S.StateT Status IO ()
searchServ = do
  svc <- req $ prompt "service name: " reqResp
  db  <- S.gets $ view database
  case pwSearch svc db of
    []  -> do
      lift $ putStrLn "\nservice not found"
      mainMenu
    [x] -> viewEdit x
    xs  -> selectServ xs

listServ :: S.StateT Status IO ()
listServ = S.gets (view database) >>= selectServ . pwSearch ""

selectServ :: [String] -> S.StateT Status IO ()
selectServ xs = menu "Select Service" $
  ("cancel", mainMenu) :
  map (\x -> (x, viewEdit x)) xs

viewEdit :: String -> S.StateT Status IO ()
viewEdit x = menu x
  [ ( "show password", showPass x >> viewEdit x )
  , ( "cancel",        mainMenu                 )
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

buildData :: S.StateT Status IO PWData
buildData = do
  d <- S.StateT $ return . newPWData
  req $ reqIf (confirm "would you like to change the default policy?")
    (do
      let p = d^.pwPolicy
      p <- editPolicy p <|> do
        reqIO $ putStrLn "invalid password policy - using default"
        return p
      return $ set pwPolicy p d)
    (return d)

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

menu
  :: String
  -> [(String, S.StateT Status IO a)]
  -> S.StateT Status IO a
menu title = reqState . prompt ("\n*** " ++ title ++ " ***") .
  reqMenu . map menuItem

menuItem :: (String, a) -> (String, Request a)
menuItem (str, x) = (str, return x)

reqState :: Request (S.StateT s IO a) -> S.StateT s IO a
reqState = join . req

req :: Request a -> S.StateT s IO a
req = lift . fmap fromJust . runRequest . required

tryReq :: Request a -> S.StateT s IO (Maybe a)
tryReq = lift . runRequest

confirm :: String -> Request Bool
confirm x = required $ prompt (x ++ " (y/n): ") $ reqAgree Nothing reqResp

--jl
