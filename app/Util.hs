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

module Util (menu, run, req, tryReq, confirm) where

import Control.Monad (join)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S
import Data.Maybe (fromJust)
import System.Console.HCL
  ( Request
  , prompt
  , reqAgree
  , reqIf
  , reqMenu
  , reqResp
  , required
  , runRequest
  )

import Types

menu
  :: String
  -> [(String, S.StateT Status IO a)]
  -> S.StateT Status IO a
menu title = reqState . prompt ("\n*** " ++ title ++ " ***") .
  reqMenu . map menuItem

menuItem :: (String, a) -> (String, Request a)
menuItem (str, x) = (str, return x)

reqState :: Request (S.StateT s IO a) -> S.StateT s IO a
reqState = join . req

run :: Monad m => (s -> (a, s)) -> S.StateT s m a
run f = S.StateT $ return . f

req :: Request a -> S.StateT s IO a
req = lift . fmap fromJust . runRequest . required

tryReq :: Request a -> S.StateT s IO (Maybe a)
tryReq = lift . runRequest

confirm :: String -> Request Bool
confirm x = required $ prompt (x ++ " (y/n): ") $ reqAgree Nothing reqResp

--jl
