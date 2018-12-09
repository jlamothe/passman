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
import Data.Maybe (fromJust)
import qualified System.Console.HCL as R

data Status = Status
  { _masterPass :: String
  }

makeLenses ''Status

main :: IO ()
main = do
  ms <- R.runRequest setup
  case ms of
    Nothing -> return ()
    Just s  -> S.evalStateT mainMenu s

setup :: R.Request Status
setup = fmap Status getMasterPass

getMasterPass :: R.Request String
getMasterPass = do
  p1 <- R.required $ R.prompt "master password: " R.reqPassword
  p2 <- R.required $ R.prompt "confirm master password: " R.reqPassword
  if p1 /= p2
    then do
      R.reqIO $ putStrLn "passwords do not match"
      R.reqFail
    else return p1

mainMenu :: S.StateT Status IO ()
mainMenu = do
  menu "Main Menu"
    [ ( "change master password", changeMasterPass )
    , ( "quit",                   quit             )
    ]

changeMasterPass :: S.StateT Status IO ()
changeMasterPass = do
  oldP <- S.gets $ view masterPass
  newP <- req $ R.reqDefault getMasterPass oldP
  S.modify $ set masterPass newP
  mainMenu

quit :: S.StateT Status IO ()
quit = return ()

menu
  :: String
  -> [(String, S.StateT Status IO a)]
  -> S.StateT Status IO a
menu title = reqState . R.prompt ("\n*** " ++ title ++ " ***") .
  R.reqMenu . map menuItem

menuItem :: (String, a) -> (String, R.Request a)
menuItem (str, x) = (str, return x)

reqState :: R.Request (S.StateT s IO a) -> S.StateT s IO a
reqState = join . req

req :: R.Request a -> S.StateT s IO a
req = lift . fmap fromJust . R.runRequest . R.required

--jl
