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

module Spec.PWHasService (tests) where

import qualified Data.Map as M
import System.Random (mkStdGen)
import Test.HUnit (Test (..), (~?=))

import Password

tests = TestLabel "pwHasService" $ TestList $ map test'
  [ ( "empty database", "foo",  newPWDatabase, False )
  , ( "in database",    "foo",  db,            True  )
  , ( "not found",      "quux", db,            False )
  ]

test' (label, x, db, expect) = TestLabel label $
  pwHasService x db ~?= expect

db = M.fromList
  [ ( "foo", foo )
  , ( "bar", bar )
  , ( "baz", baz )
  ]

(foo, g') = newPWData g

(bar, g'') = newPWData g'

(baz, _) = newPWData g''

g = mkStdGen 1

--jl