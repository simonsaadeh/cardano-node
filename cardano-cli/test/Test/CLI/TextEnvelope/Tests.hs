{-# LANGUAGE OverloadedStrings #-}

module Test.CLI.TextEnvelope.Tests
  ( cliTests
  ) where

import           Cardano.Prelude

import           Test.CLI.TextEnvelope.Golden.Genesis.Create
                   (golden_genesisCreate)

import qualified Hedgehog as H

cliTests :: IO Bool
cliTests =
  H.checkSequential
    $ H.Group "TextEnvelope Goldens"
        [ ("golden_genesisCreate", golden_genesisCreate)
        ]
