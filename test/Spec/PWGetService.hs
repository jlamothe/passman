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

module Spec.PWGetService (tests) where

import qualified Data.Map as M
import System.Random (mkStdGen)
import Test.HUnit (Test (..), (~?=))

import Password

tests = TestLabel "pwGetService" $ TestList
  [ empty, found, notFound ]

empty = TestLabel "empty database" $
  pwGetService "foo" newPWDatabase ~?= Nothing

found = TestLabel "service found" $
  pwGetService "foo" db ~?= Just foo

notFound = TestLabel "service not found" $
  pwGetService "quux" db ~?= Nothing

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
