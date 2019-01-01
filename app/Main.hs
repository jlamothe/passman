{-

passman
Copyright (C) 2018, 2019 Jonathan Lamothe
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

module Main where

import Control.Monad (mapM_)
import Control.Monad.Trans.State as S
import Data.Maybe (maybe)
import System.Console.HCL (Request, reqFail, reqIO, runRequest)
import System.Environment (lookupEnv)
import System.Random (getStdGen)

import Password

import Types
import UI
import Util

main :: IO ()
main = runRequest setup >>= mapM_ (S.evalStateT mainMenu)

setup :: Request Status
setup = do
  g  <- reqIO getStdGen
  p  <- getDBPath
  db <- loadFrom p
  pw <- getMasterPass
  return $ Status g pw p db

getDBPath :: Request FilePath
getDBPath = reqIO (lookupEnv "HOME") >>= maybe
  (do
    reqIO $ putStrLn "ERROR: can't find home directory"
    reqFail)
  (\home -> case pathDelim home of
    Nothing -> do
      reqIO $ putStrLn "ERROR: unsupported home path"
      reqFail
    Just delim -> return $ home ++
      (if last home == delim then "" else [delim]) ++
      ".passman.json")

pathDelim :: FilePath -> Maybe Char
pathDelim = foldr
  (\x a -> case x of
    '/'  -> Just '/'
    '\\' -> Just '\\'
    _    -> a)
  Nothing

--jl
