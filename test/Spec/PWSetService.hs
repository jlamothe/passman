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

module Spec.PWSetService (tests) where

import qualified Data.Map as M
import System.Random (mkStdGen, StdGen)
import Test.HUnit (Test (..), (~?=))

import Password

tests :: Test
tests = TestLabel "pwSetService" $ TestList
  [ addToEmpty, addToNonEmpty, addToExisting ]

addToEmpty :: Test
addToEmpty = tests' "empty database" newPWDatabase 1

addToNonEmpty :: Test
addToNonEmpty = tests' "non-empty database" nonEmpty 3

addToExisting :: Test
addToExisting = tests' "existing database" existing 3

tests' :: String -> PWDatabase -> Int -> Test
tests' label db size = TestLabel label $ TestList
  [ dbSize result size
  , find result
  ] where
    result = pwSetService "foo" foo db

dbSize :: M.Map String PWData -> Int -> Test
dbSize db expect = TestLabel "database size" $
  length db ~?= expect

find :: M.Map String PWData -> Test
find db = TestLabel "record" $
  M.lookup "foo" db ~?= Just foo

nonEmpty :: M.Map String PWData
nonEmpty = M.fromList
  [ ( "bar", bar )
  , ( "baz", baz )
  ]

existing :: M.Map String PWData
existing = M.fromList
  [ ( "foo", foo' )
  , ( "bar", bar  )
  , ( "baz", baz  )
  ]

foo :: PWData
g1  :: StdGen
(foo, g1) = newPWData g

foo' :: PWData
g2   :: StdGen
(foo', g2) = newPWData g1

bar :: PWData
g3  :: StdGen
(bar, g3) = newPWData g2

baz :: PWData
(baz, _) = newPWData g3

g :: StdGen
g = mkStdGen 1

--jl
