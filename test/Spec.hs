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

module Main where

import Control.Monad (when)
import System.Exit (exitFailure)
import Test.HUnit (errors, failures, runTestTT, Test(TestList))

import qualified Spec.JSON as JSON
import qualified Spec.NewPWData as NewPWData
import qualified Spec.NewPWDatabase as NewPWDatabase
import qualified Spec.NewPWPolicy as NewPWPolicy
import qualified Spec.NewPWSalt as NewPWSalt
import qualified Spec.PWGenerate as PWGenerate
import qualified Spec.PWGetService as PWGetService
import qualified Spec.PWHasService as PWHasService
import qualified Spec.PWRemoveService as PWRemoveService
import qualified Spec.PWSearch as PWSearch
import qualified Spec.PWSetService as PWSetService
import qualified Spec.ValidatePWData as ValidatePWData
import qualified Spec.ValidatePWDatabase as ValidatePWDatabase
import qualified Spec.ValidatePWPolicy as ValidatePWPolicy

main :: IO ()
main = do
  counts <- runTestTT tests
  when (failures counts > 0 || errors counts > 0)
    exitFailure

tests :: Test
tests = TestList
  [ NewPWDatabase.tests
  , NewPWData.tests
  , NewPWPolicy.tests
  , NewPWSalt.tests
  , ValidatePWDatabase.tests
  , ValidatePWData.tests
  , ValidatePWPolicy.tests
  , PWGenerate.tests
  , PWHasService.tests
  , PWSetService.tests
  , PWGetService.tests
  , PWSearch.tests
  , PWRemoveService.tests
  , JSON.tests
  ]

--jl
