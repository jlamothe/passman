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

module Spec.PWRemoveService (tests) where

import qualified Data.Map.Lazy as M
import System.Random (mkStdGen, StdGen)
import Test.HUnit (Test (..), assertBool, (~?=))

import Password

tests :: Test
tests = TestLabel "pwRemoveService" $ TestList
  [ emptyDB
  , existingService
  , missingService
  ]

emptyDB :: Test
emptyDB = TestLabel "empty database" $
  pwRemoveService "foo" newPWDatabase ~?= newPWDatabase

existingService :: Test
existingService = TestLabel "existing service" $
  test' "foo" ["bar", "baz"]

missingService :: Test
missingService = TestLabel "missing service" $
  test' "quux" ["foo", "bar", "baz"]

test' :: String -> [String] -> Test
test' serv keys = let db' = pwRemoveService serv db in
  TestList $
  TestLabel "key count" (length keys ~?= length (M.keys db')) :
  map
  (\x -> TestLabel x $ TestCase $ assertBool "service missing" $ pwHasService x db')
  keys

db :: M.Map String PWData
db = M.fromList
  [ ( "foo", foo )
  , ( "bar", bar )
  , ( "baz", baz )
  ]

foo :: PWData
g'  ::StdGen
(foo, g') = newPWData g

bar :: PWData
g'' :: StdGen
(bar, g'') = newPWData g'

baz :: PWData
(baz, _) = newPWData g''

g :: StdGen
g = mkStdGen 1

--jl
