{-|

Module:      Password
Description: a simple password manager
Copyright:   (C) 2018 Jonathan Lamothe
License:     LGPLv3 (or later)
Maintainer:  jlamothe1980@gmail.com

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

{-# LANGUAGE TemplateHaskell #-}

module Password (
  -- * Data Types
  PWPolicy (..),
  -- ** Lenses
  -- $lenses
  pwLength, pwUpper, pwLower, pwDigits, pwSpecial,
  -- ** Default Instances
  newPWPolicy,
  -- * Functions
  validatePWPolicy, applyPWPolicy
  ) where

import Control.Lens (makeLenses, (^.))
import Data.Char (isUpper, isLower, isDigit, isAlphaNum)
import Data.Maybe (fromMaybe)

-- | defines a password policy
data PWPolicy = PWPolicy
  { _pwLength :: Int
  -- ^ password length
  , _pwUpper :: Int
  -- ^ the minimum number of upper case characters
  , _pwLower :: Int
  -- ^ the minimum number of lower case characters
  , _pwDigits :: Int
  -- ^ the minimum number of digits
  , _pwSpecial :: Maybe Int
  -- ^ the minimum number of non-alphanumeric characters (not allowed
  -- if @"Nothing"@)
  } deriving (Eq, Show)

-- $lenses The following functions are automatically generated by
-- @makeLenses@. See the
-- [lens](http://hackage.haskell.org/package/lens) package for further
-- details.

makeLenses ''PWPolicy

-- | default password policy
newPWPolicy :: PWPolicy
newPWPolicy = PWPolicy 16 0 0 0 (Just 0)

-- | validates a password policy
validatePWPolicy
  :: PWPolicy
  -- ^ the policy being validated
  -> Bool
  -- ^ indicates whether or not the policy is valid
validatePWPolicy x = and
  [ needed <= x^.pwLength
  , x^.pwLength >= 0
  , x^.pwUpper >= 0
  , x^.pwLower >= 0
  , x^.pwDigits >= 0
  , fromMaybe 0 (x^.pwSpecial) >= 0
  ] where
    needed = x^.pwUpper + x^.pwLower + x^.pwDigits + special
    special = fromMaybe 0 $ x^.pwSpecial

-- | checks whether or not a password meets a given password policy
applyPWPolicy
  :: String
  -- ^ the password
  -> PWPolicy
  -- ^ the policy
  -> Bool
  -- ^ @"True"@ if the password meets the policy, @"False"@ otherwise
applyPWPolicy pw policy = and
  [ length pw <= policy^.pwLength
  , length (filter isUpper pw) >= policy^.pwUpper
  , length (filter isLower pw) >= policy^.pwLower
  , length (filter isDigit pw) >= policy^.pwDigits
  , length (filter (not . isAlphaNum) pw) >=
    fromMaybe (succ $ policy^.pwLength) (policy^.pwSpecial)
  , validatePWPolicy policy
  ]

--jl
