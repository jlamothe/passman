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

module Spec.ValidatePWPolicy (tests) where

import Control.Lens (set)
import Test.HUnit (Test(..), (~?=))

import Password

tests = TestLabel "validatePWPolicy" $ TestList $ map test'
  [ ( "default",          id,                    True  )
  , ( "no special chars", set pwSpecial Nothing, True  )
  , ( "valid minimums",   validMins,             True  )
  , ( "excessive upper",  set pwUpper 99,        False )
  , ( "excessive lower",  set pwLower 99,        False )
  , ( "excessive digits", set pwDigits 99,       False )
  , ( "excessive total",  excessive,             False )
  , ( "short",            set pwLength 8,        True  )
  , ( "short valid",      shortValid,            True  )
  , ( "short invalid",    shortInvalid,          False )
  ]

test' (label, f, expect) = TestLabel label $
  validatePWPolicy x ~?= expect where
  x = f newPWPolicy

validMins = setAll 1

excessive = setAll 5

shortValid = set pwLength 8 . setAll 2

shortInvalid = set pwLength 8 . set pwUpper 9

setAll x = set pwUpper x .
  set pwLower x .
  set pwDigits x .
  set pwSpecial (Just x)

--jl
