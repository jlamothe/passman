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

{-# LANGUAGE TemplateHaskell #-}

module Types (Status (Status), gen, masterPass, database) where

import Control.Lens (makeLenses, set, (^.))
import System.Random (RandomGen (next, split), StdGen)

import Password

data Status = Status
  { _gen        :: StdGen
  , _masterPass :: String
  , _database   :: PWDatabase
  }

makeLenses ''Status

instance RandomGen Status where
  next s = (x, s') where
    (x, g') = next g
    s' = set gen g' s
    g = s^.gen
  split s = (s1, s2) where
    s1 = set gen g1 s
    s2 = set gen g2 s
    (g1, g2) = split g
    g = s^.gen

--jl
