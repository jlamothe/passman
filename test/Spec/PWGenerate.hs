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

module Spec.PWGenerate (tests) where

import Data.Maybe (fromJust)
import Lens.Micro (set, (^.))
import System.Random (mkStdGen, StdGen)
import Test.HUnit
  (Test (..)
  , assertBool
  , assertEqual
  , assertFailure
  , (~?=)
  )

import Password

tests :: Test
tests = TestLabel "pwGenerate" $ TestList
  [ defaultData
  , invalidPolicy
  , constraints
  , noSpecial
  , differentMaster
  ]

defaultData :: Test
defaultData = TestLabel "default data" $ TestCase $
  case pwGenerate "foo" validData of
    Nothing -> assertFailure "no password generated"
    Just x  -> assertEqual "incorrect password length"
      (validData^.pwPolicy.pwLength) (length x)

invalidPolicy :: Test
invalidPolicy = TestLabel "invalid policy" $
  pwGenerate "foo" invalidPolicy' ~?= Nothing

constraints :: Test
constraints = TestLabel "strict constraints" $ TestCase $
  case pwGenerate "foo" constraints' of
    Nothing -> assertFailure "no password generated"
    Just x  -> do
      assertEqual "incorrect password length"
        (constraints'^.pwPolicy.pwLength) (length x)
      assertEqual "incorrect number of upper case"
        (constraints'^.pwPolicy.pwUpper) (pwCountUpper x)
      assertEqual "incorrect number of lower case"
        (constraints'^.pwPolicy.pwLower) (pwCountLower x)
      assertEqual "incorrect number of digita"
        (constraints'^.pwPolicy.pwDigits) (pwCountDigits x)
      assertEqual "incorrect number of special characters"
        (fromJust $ constraints'^.pwPolicy.pwSpecial) (pwCountSpecial x)

noSpecial :: Test
noSpecial = TestLabel "no special chars" $ TestCase $
  case pwGenerate "foo" noSpecial' of
    Nothing -> assertFailure "no password generated"
    Just x  -> do
      assertEqual "incorrect password length"
        (noSpecial'^.pwPolicy.pwLength) (length x)
      assertEqual "special characters found" 0 $ pwCountSpecial x

differentMaster :: Test
differentMaster = TestLabel "different master passwords" $ TestCase $
  assertBool "passwords match" $
  fromJust (pwGenerate "foo" validData) /=
  fromJust (pwGenerate "bar" validData)

validData :: PWData
(validData, _) = newPWData g

invalidPolicy' :: PWData
invalidPolicy' = set (pwPolicy.pwLength) (-1) validData

constraints' :: PWData
constraints' = set (pwPolicy.pwUpper) 4 $
  set (pwPolicy.pwLower) 4 $
  set (pwPolicy.pwDigits) 4 $
  set (pwPolicy.pwSpecial) (Just 4)
  validData

noSpecial' :: PWData
noSpecial' = set (pwPolicy.pwLength) 256 $
  set (pwPolicy.pwSpecial) Nothing
  validData

g :: StdGen
g = mkStdGen 1

--jl
