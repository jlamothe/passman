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

import Control.Lens (makeLenses, set, view)
import qualified Control.Monad.Trans.State as S
import Control.Monad (join)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (mapM_)
import Data.Maybe (fromJust)
import System.Console.HCL
  ( Request (..)
  , prompt
  , reqDefault
  , reqFail
  , reqIO
  , reqMenu
  , reqPassword
  , required
  , runRequest
  )
import System.Random (StdGen, getStdGen)

import Password

data Status = Status
  { _gen        :: StdGen
  , _masterPass :: String
  , _database   :: PWDatabase
  }

makeLenses ''Status

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
    [ ( "change master password", changeMasterPass )
    , ( "lock session",           lockSession      )
    , ( "quit",                   quit             )
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

--jl
