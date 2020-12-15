{-

passman
Copyright (C) 2018-2020 Jonathan Lamothe
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

module Spec.ValidatePWData (tests) where

import Control.Lens (set)
import qualified Data.ByteString.Lazy as B
import System.Random (mkStdGen, StdGen)
import Test.HUnit (Test (..), (~?=))

import Password

tests :: Test
tests = TestLabel "validatePWData" $ TestList $ map test'
  [ ( "valid",          new,           True  )
  , ( "invalid policy", invalidPolicy, False )
  , ( "invalid salt",   invalidSalt,   False )
  ]

test' :: (String, PWData, Bool) -> Test
test' (label, x, expect) = TestLabel label $
  validatePWData x ~?= expect

new :: PWData
(new, _) = newPWData g

invalidPolicy :: PWData
invalidPolicy = set (pwPolicy.pwLength) (-1) new

invalidSalt :: PWData
invalidSalt = set pwSalt (PWSalt B.empty) new

g :: StdGen
g = mkStdGen 1

--jl
