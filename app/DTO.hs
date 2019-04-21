{-# LANGUAGE DataKinds, DeriveAnyClass, FlexibleInstances, OverloadedStrings, MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}
module DTO where

import Data.Text
import Data.UUID
import Database.DynamoDB.TH
import Database.DynamoDB.Types

data LL = LL
  { _latitude :: Double
  , _longitude :: Double
  } deriving (Read, Show)

defaultLL :: LL
defaultLL = LL
  { _latitude = 0
  , _longitude = 0
  }

data Test = Test {
    sampleKey :: Text
  , user      :: Text
  , subject   :: Text
  , replies   :: Int
  , id        :: UUID
  , maybeVal  :: Maybe Text
  , latLng    :: LL
  } deriving (Show)

-- Generate instances and category', user' etc. variables for queries/updates
mkTableDefs "migrate" (tableConfig "seekoss-serverless-test-dev-" (''Test, WithRange) [] [])
deriveCollection ''LL defaultTranslate