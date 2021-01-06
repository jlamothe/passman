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

module Spec.ValidatePWPolicy (tests) where

import Lens.Micro (set)
import Test.HUnit (Test(..), (~?=))

import Password

tests :: Test
tests = TestLabel "validatePWPolicy" $ TestList $ map test'
  [ ( "default",          id,                        True  )
  , ( "no special chars", set pwSpecial Nothing,     True  )
  , ( "valid minimums",   validMins,                 True  )
  , ( "excessive upper",  set pwUpper 99,            False )
  , ( "excessive lower",  set pwLower 99,            False )
  , ( "excessive digits", set pwDigits 99,           False )
  , ( "excessive total",  excessive,                 False )
  , ( "short",            set pwLength 8,            True  )
  , ( "short valid",      shortValid,                True  )
  , ( "short invalid",    shortInvalid,              False )
  , ( "negative length",  set pwLength (-1),         False )
  , ( "negative upper",   set pwUpper (-1),          False )
  , ( "negative lower",   set pwLower (-1),          False )
  , ( "negative digits",  set pwDigits (-1),         False )
  , ( "negative special", set pwSpecial (Just (-1)), False )
  ]

test' :: (String, PWPolicy -> PWPolicy, Bool) -> Test
test' (label, f, expect) = TestLabel label $
  validatePWPolicy x ~?= expect where
  x = f newPWPolicy

validMins :: PWPolicy -> PWPolicy
validMins = setAll 1

excessive :: PWPolicy -> PWPolicy
excessive = setAll 5

shortValid :: PWPolicy -> PWPolicy
shortValid = set pwLength 8 . setAll 2

shortInvalid :: PWPolicy -> PWPolicy
shortInvalid = set pwLength 8 . set pwUpper 9

setAll :: Int -> PWPolicy -> PWPolicy
setAll x = set pwUpper x .
  set pwLower x .
  set pwDigits x .
  set pwSpecial (Just x)

--jl
