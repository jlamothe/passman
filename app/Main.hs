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

module Main where

import System.Console.HCL
  ( Request (..)
  , execReq
  , prompt
  , reqFail
  , reqIO
  , reqPassword
  , required
  )

main :: IO ()
main = execReq getMasterPass

getMasterPass :: Request String
getMasterPass = do
  p1 <- required $ prompt "master password: " reqPassword
  p2 <- required $ prompt "confirm master password: " reqPassword
  if p1 /= p2
    then do
      reqIO $ putStrLn "passwords do not match"
      reqFail
    else return p1

--jl
