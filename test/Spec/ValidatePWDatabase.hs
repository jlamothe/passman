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

module Spec.ValidatePWDatabase (tests) where

import Control.Lens (set)
import qualified Data.Map as M
import System.Random (mkStdGen, StdGen)
import Test.HUnit (Test (..), (~?=))

import Password

tests :: Test
tests = TestLabel "validatePWDatabase" $ TestList $ map test'
  [ ( "empty",       newPWDatabase, True  )
  , ( "valid",       validDB,       True  )
  , ( "foo invalid", fooInvalid,    False )
  , ( "bar invalid", barInvalid,    False )
  ]

test' :: (String, PWDatabase, Bool) -> Test
test' (label, x, expect) = TestLabel label $
  validatePWDatabase x ~?= expect

validDB :: M.Map String PWData
validDB = M.fromList [("foo", validData), ("bar", validData)]

fooInvalid :: M.Map String PWData
fooInvalid = M.insert "foo" invalidData validDB

barInvalid :: M.Map String PWData
barInvalid = M.insert "bar" invalidData validDB

validData :: PWData
(validData, _) = newPWData g

invalidData :: PWData
invalidData = set (pwPolicy.pwLength) (-1) validData

g :: StdGen
g = mkStdGen 1

--jl
