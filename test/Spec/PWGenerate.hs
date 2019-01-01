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

module Spec.PWGenerate (tests) where

import Control.Lens (set, (^.))
import Data.Maybe (fromJust)
import System.Random (mkStdGen)
import Test.HUnit
  (Test (..)
  , assertBool
  , assertEqual
  , assertFailure
  , (~?=)
  )

import Password

tests = TestLabel "pwGenerate" $ TestList
  [ defaultData
  , invalidPolicy
  , constraints
  , noSpecial
  , differentMaster
  ]

defaultData = TestLabel "default data" $ TestCase $
  case pwGenerate "foo" validData of
    Nothing -> assertFailure "no password generated"
    Just x  -> assertEqual "incorrect password length"
      (validData^.pwPolicy.pwLength) (length x)

invalidPolicy = TestLabel "invalid policy" $
  pwGenerate "foo" invalidPolicy' ~?= Nothing

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

noSpecial = TestLabel "no special chars" $ TestCase $
  case pwGenerate "foo" noSpecial' of
    Nothing -> assertFailure "no password generated"
    Just x  -> do
      assertEqual "incorrect password length"
        (noSpecial'^.pwPolicy.pwLength) (length x)
      assertEqual "special characters found" 0 $ pwCountSpecial x

differentMaster = TestLabel "different master passwords" $ TestCase $
  assertBool "passwords match" $
  fromJust (pwGenerate "foo" validData) /=
  fromJust (pwGenerate "bar" validData)

(validData, _) = newPWData g

invalidPolicy' = set (pwPolicy.pwLength) (-1) validData

constraints' = set (pwPolicy.pwUpper) 4 $
  set (pwPolicy.pwLower) 4 $
  set (pwPolicy.pwDigits) 4 $
  set (pwPolicy.pwSpecial) (Just 4)
  validData

noSpecial' = set (pwPolicy.pwLength) 256 $
  set (pwPolicy.pwSpecial) Nothing
  validData

g = mkStdGen 1

--jl
