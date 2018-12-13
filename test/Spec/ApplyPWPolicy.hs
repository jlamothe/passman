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

module Spec.ApplyPWPolicy (tests) where

import Control.Lens (set)
import Test.HUnit (Test(..), (~?=))

import Password

tests = TestLabel "applyPWPolicy" $ TestList $ map test'
  [ ( "default pass",         "password",           id,             True  )
  , ( "too long",             take 99 $ repeat 'x', id,             False )
  , ( "insufficient upper",   "password",           set pwUpper 1,  False )
  , ( "sufficient upper",     "Password",           set pwUpper 1,  True  )
  , ( "insufficient lower",   "PASSWORD",           set pwLower 1,  False )
  , ( "sufficient lower",     "password",           set pwLower 1,  True  )
  , ( "insufficient digits",  "password",           set pwDigits 1, False )
  , ( "sufficient digits",    "password1",          set pwDigits 1, True  )
  , ( "insufficient special", "Password1",          spec (Just 1),  False )
  , ( "sufficient special",   "Password1/",         spec (Just 1),  True  )
  , ( "illegal special",      "Password1/",         spec Nothing,   False )
  , ( "bad policy",           "password",           badPolicy,      False )
  ]

test' (label, pw, f, expect) = TestLabel label $
  applyPWPolicy pw (f newPWPolicy) ~?= expect

spec = set pwSpecial

badPolicy = set pwUpper (-1)

--jl
