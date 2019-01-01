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

module Util
  ( menu
  , run
  , withService
  , ifServExists
  , setService
  , req
  , tryReq
  , confirm
  , loadFrom
  , save
  ) where

import Control.Lens (over, view)
import Control.Monad (join)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S
import Data.Aeson (decodeFileStrict, encodeFile)
import Data.Maybe (fromJust)
import System.Console.HCL
  ( Request
  , prompt
  , reqAgree
  , reqChar
  , reqDefault
  , reqIf
  , reqIO
  , reqMenu
  , required
  , runRequest
  )

import Password

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

withService
  :: String
  -> S.StateT Status IO a
  -> (PWData -> S.StateT Status IO a)
  -> S.StateT Status IO a
withService srv fb act = do
  db <- S.gets $ view database
  case pwGetService srv db of
    Nothing -> fb
    Just x  -> act x

ifServExists
  :: String
  -> S.StateT Status IO a
  -> S.StateT Status IO a
  -> S.StateT Status IO a
ifServExists s x y = do
  db <- S.gets $ view database
  if pwHasService s db
    then x
    else y

setService :: String -> PWData -> S.StateT Status IO ()
setService k = S.modify . over database . pwSetService k

req :: Request a -> S.StateT s IO a
req = lift . fmap fromJust . runRequest . required

tryReq :: Request a -> S.StateT s IO (Maybe a)
tryReq = lift . runRequest

confirm :: String -> Request Bool
confirm x = prompt (x ++ " (y/n): ") $ reqAgree Nothing $ fmap return reqChar

loadFrom :: FilePath -> Request PWDatabase
loadFrom path = reqDefault
  (reqIO (decodeFileStrict path))
  (Just newPWDatabase) >>= maybe
  (return newPWDatabase)
  return

save :: S.StateT Status IO ()
save = do
  path <- S.gets $ view dbPath
  db   <- S.gets $ view database
  lift $ encodeFile path db

--jl
