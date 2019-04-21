{-# LANGUAGE DataKinds, FlexibleInstances, OverloadedStrings, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import AWSLambda.Events.APIGateway
import Control.Lens
import Control.Monad.IO.Class
import Network.AWS.Data
import Data.Aeson
import Data.Aeson.Embedded
import Data.HashMap.Strict

import Network.AWS (Credentials(..), newEnv, LogLevel(..), newLogger, envLogger, runResourceT, Region, envAuth, send, setEndpoint, Service, MonadAWS, AWS)
import System.IO
import Control.Monad.Trans.AWS (runAWST, reconfigure, configure, Env, AWST', HasEnv)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import Data.Text (Text)
import System.Environment (getEnvironment, getEnv, lookupEnv)
import Data.Maybe (isJust)
import Network.AWS.DynamoDB (piItem, avS, attributeValue, dynamoDB, AttributeValue)
import Database.DynamoDB (putItem)
import Data.UUID (UUID(..), nil)
import Data.Time (formatTime)
import Data.Time.Calendar (Day(..))
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime, LocalTime(..), TimeOfDay(..))
import qualified Network.AWS.DynamoDB.Types as D
import Network.HTTP.Types
import DTO

main = apiGatewayMain handler

handler :: APIGatewayProxyRequest (Embedded Value) -> IO (APIGatewayProxyResponse (Embedded Value))
handler request = mkCustomEnv >>= \env -> runResourceT . runAWST env $ switchOnRequest request

switchOnRequest :: APIGatewayProxyRequest (Embedded Value) -> AWS (APIGatewayProxyResponse (Embedded Value))
switchOnRequest req
  | methodPost == req ^. agprqHttpMethod =
      createHH req
  | methodPut == req ^. agprqHttpMethod =
      createHH req
  | methodDelete == req ^. agprqHttpMethod =
      createHH req
  | methodGet == req ^. agprqHttpMethod =
      createHH req
  | otherwise =
      pure $ responseNotFound & responseBodyEmbedded ?~ Null

createHH :: APIGatewayProxyRequest (Embedded Value) -> AWS (APIGatewayProxyResponse (Embedded Value))
createHH req = do
   putItem (Test "news" "john" "test" 20 nil Nothing defaultLL)
   return (responseOK & responseBodyEmbedded ?~ Null)
   -- pure $ responseOK & responseBodyEmbedded ?~ [1, 2, 3]

mkCustomEnv :: IO Env
mkCustomEnv = do
  tableName <- Text.pack <$> getEnv "DYNAMODB_TABLE"
  isOffline <- isJust <$> lookupEnv "IS_OFFLINE"
  lgr <- newLogger Debug stdout
  newEnv Discover
      <&> set envLogger lgr
      <&> configure (mkTargetDB isOffline)

mkTargetDB :: Bool -> Service
mkTargetDB isOffline =
  if isOffline
    then setEndpoint False "localhost" 8000 dynamoDB
    else dynamoDB -- it'll auto figure it out when on AWS

