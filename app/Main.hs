{-

passman
Copyright (C) 2018-2021 Jonathan Lamothe
<jonathan@jlamothe.net>

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

import Control.Monad.Trans.State as S
import System.Console.HCL (Request, reqIO, runRequest)
import System.EasyFile
  ( createDirectoryIfMissing
  , getAppUserDataDirectory
  , (</>)
  )
import System.Random (getStdGen)

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
getDBPath = reqIO $ do
  path <- getAppUserDataDirectory "passman"
  createDirectoryIfMissing True path
  return $ path </> "database.json"

--jl
