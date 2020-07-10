{-# LANGUAGE OverloadedStrings    #-}

module Test.CLI.TextEnvelope.Golden.Genesis.Create
  ( golden_genesisCreate
  ) where

import Cardano.Prelude hiding (to)

import Data.Maybe
  ( fromJust
  )
import Control.Lens
  ( (^?)
  , (^..)
  , each
  )
import Prelude(String) 

import qualified Control.Lens         as CL
import qualified Data.Aeson           as J
import qualified Data.Aeson.Lens      as J
import qualified Data.HashMap.Strict  as HMS
import qualified Data.List            as L
import qualified Data.Set             as S
import qualified Data.Text            as T
import qualified Data.Time.Clock      as DT
import qualified Data.Time.Format     as DT
import qualified Data.Vector          as DV
import qualified System.Directory     as IO
import qualified System.IO            as IO
import qualified System.IO.Temp       as IO

import Hedgehog
  ( Property
  , forAll
  , (===)
  )

import qualified Hedgehog       as H
import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R
import qualified Test.OptParse  as OP

{- HLINT ignore "Use camelCase" -}

-- | Convert an object to an array of array.
--
-- For example: {"a": 1, "b": 2} -> [["a", 1], ["b", 2]]
objectToArray :: J.Value -> J.Value
objectToArray v = case v of
  J.Object o -> J.Array (DV.fromList (fmap (\(ek, ev) -> J.Array (DV.fromList [J.String ek, ev])) (HMS.toList o)))
  a -> a

-- | Assert the file contains the given number of occurences of the given string
assertFileOccurences :: Int -> String -> FilePath -> H.PropertyT IO ()
assertFileOccurences n s fp = do
  signingKeyContents <- liftIO $ IO.readFile fp

  length (filter (s `L.isInfixOf`) (L.lines signingKeyContents)) === n

-- | Format the given time as an ISO 8601 date-time string
formatIso8601 :: DT.UTCTime -> String
formatIso8601 = DT.formatTime DT.defaultTimeLocale (DT.iso8601DateFormat (Just "%H:%M:%SZ"))

-- | Return the supply value with the result of the supplied function as a tuple
withSnd :: (a -> b) -> a -> (a, b)
withSnd f a = (a, f a)

golden_genesisCreate :: Property
golden_genesisCreate = OP.propertyOnce $ do
  liftIO $ IO.createDirectoryIfMissing True "tmp"
  tempDir <- liftIO $ IO.createTempDirectory "tmp" "test"
  let genesisFile = tempDir <> "/genesis.json"
  let cleanupPaths = [tempDir]
  
  fmtStartTime <- fmap formatIso8601 $ liftIO DT.getCurrentTime

  (supply       , fmtSupply       ) <- fmap (withSnd show) $ forAll $ G.int (R.linear 10000000 4000000000)
  (delegateCount, fmtDelegateCount) <- fmap (withSnd show) $ forAll $ G.int (R.linear 4 19)
  (utxoCount    , fmtUtxoCount    ) <- fmap (withSnd show) $ forAll $ G.int (R.linear 4 19)

  -- Create the genesis json file and required keys
  OP.execCardanoCLIParser cleanupPaths $
    OP.evalCardanoCLIParser
      [ "shelley","genesis","create"
      , "--testnet-magic", "12"
      , "--start-time", fmtStartTime
      , "--supply", fmtSupply
      , "--gen-genesis-keys", fmtDelegateCount
      , "--gen-utxo-keys", fmtUtxoCount
      , "--genesis-dir", tempDir
      ]

  OP.assertFilesExist [genesisFile]

  genesisContents <- liftIO $ IO.readFile genesisFile

  H.annotate genesisContents

  actualSupply        <- forAll $ pure $ fromJust $ genesisContents ^? J.key "maxLovelaceSupply" . J._Integral
  actualStartTime     <- forAll $ pure $ fromJust $ genesisContents ^? J.key "systemStart" . J._String <&> T.unpack
  actualDelegateCount <- forAll $ pure $ fromJust $ genesisContents ^? J.key "genDelegs" . J._Object <&> HMS.size
  actualTotalSupply   <- forAll $ pure $ sum      $ genesisContents ^.. J.key "initialFunds" . J._Object . CL.to HMS.toList . each . CL._2 . J._Integral
  actualDelegates     <- forAll $ pure $ fromJust $ genesisContents ^? J.key "genDelegs" . CL.to objectToArray . J._Array

  actualSupply        === supply
  actualStartTime     === fmtStartTime
  actualDelegateCount === delegateCount
  actualDelegateCount === utxoCount
  actualTotalSupply   === supply        -- Check that the sum of the initial fund amounts matches the total supply

  -- Check uniqueness and count of hash keys
  let hashKeys = actualDelegates ^.. each . J.nth 0 . J._String

  S.size (S.fromList hashKeys) === length hashKeys -- This isn't strictless necessary because we use aeson which guarantees uniqueness of keys
  S.size (S.fromList hashKeys) === delegateCount

  -- Check uniqueness and count of hash keys
  let delegateKeys = actualDelegates ^.. each . J.nth 1 . J.key "delegate" . J._String

  S.size (S.fromList delegateKeys) === length delegateKeys
  S.size (S.fromList delegateKeys) === delegateCount

  for_ [1 .. delegateCount] $ \i -> do
    -- Check Genesis keys
    assertFileOccurences 1 "Genesis signing key"      $ tempDir <> "/genesis-keys/genesis" <> show i <> ".skey"
    assertFileOccurences 1 "Genesis verification key" $ tempDir <> "/genesis-keys/genesis" <> show i <> ".vkey"

    -- Check delegate keys
    assertFileOccurences 1 "Node operator signing key"                  $ tempDir <> "/delegate-keys/delegate" <> show i <> ".skey"
    assertFileOccurences 1 "Node operator verification key"             $ tempDir <> "/delegate-keys/delegate" <> show i <> ".vkey"
    assertFileOccurences 1 "Node operational certificate issue counter" $ tempDir <> "/delegate-keys/delegate" <> show i <> ".counter"

    -- Check utxo keys
    assertFileOccurences 1 "Genesis UTxO signing key"       $ tempDir <> "/utxo-keys/utxo" <> show i <> ".skey"
    assertFileOccurences 1 "Genesis UTxO verification key"  $ tempDir <> "/utxo-keys/utxo" <> show i <> ".vkey"
