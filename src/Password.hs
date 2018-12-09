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

module Password (
  -- * Data Types
  PWPolicy (..)
  ) where

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

--jl
