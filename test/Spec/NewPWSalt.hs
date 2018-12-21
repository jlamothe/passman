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

module Spec.NewPWSalt (tests) where

import qualified Data.ByteString.Lazy as B
import System.Random (mkStdGen)
import Test.HUnit (Test(..), assertBool, (~?=))

import Password

tests = TestLabel "newPWSalt" $ TestList
  [ testLength salt
  , testDiff salt salt'
  ] where
    (salt, g') = newPWSalt g
    (salt', _) = newPWSalt g'
    g = mkStdGen 1

testLength x = TestLabel "salt length" $ B.length x ~?= 256

testDiff x y = TestLabel "different generators" $ TestCase $
  assertBool "salts match" $ x /= y

--jl
