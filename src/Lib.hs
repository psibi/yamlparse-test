{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.Text (Text)
import qualified Data.Text.IO as T
import YamlParse.Applicative
import Control.Monad (when)

instance YamlSchema Configuration where
  yamlSchema = objectParser "Configuration" configurationParser2

data Configuration = Configuration
  { confUrl :: Maybe Text,
    confPort :: Int,
    confToken :: Text
  }
  deriving (Show, Eq)

configurationParser :: ObjectParser Configuration
configurationParser = do
  confUrl :: Maybe Text <- optionalField "url" "The url to host the server at. It will be hosted on 'localhost' by default."
  confPort <- optionalFieldWithDefault "port" 8000 "The post to host the server at."
  confToken <- requiredField "token" "The authorisation token that clients can use to authenticate themselves."
  pure Configuration {..}

configurationParser2 :: ObjectParser Configuration
configurationParser2 =
  Configuration
    <$> optionalField
      "url"
      "The url to host the server at. It will be hosted on 'localhost' by default."
    <*> optionalFieldWithDefault
      "port"
      8000
      "The post to host the server at."
    <*> requiredField
      "token"
      "The authorisation token that clients can use to authenticate themselves."

-- Example of validation
validPort :: Int -> Bool
validPort x = x < 3000

eitherValidPort :: Configuration -> Either String Configuration
eitherValidPort conf = if validPort (confPort conf)
                       then Right conf
                       else Left "Port should be less than 3000"

configurationParser3 :: ObjectParser Configuration
configurationParser3 = eitherParser eitherValidPort configurationParser2

-- Implement Same as configurationParser3 but with more precise types

isValidPort :: Int -> Either String Int
isValidPort port = if validPort port
                   then Right port
                   else Left "Port should be less than 3000"

eitherPort :: Configuration -> Either String Configuration
eitherPort conf = do
  validPort <- isValidPort (confPort conf)
  pure conf{confPort = validPort}

eitherPort2 :: Configuration -> Either String Configuration
eitherPort2 conf = (\x -> conf{confPort = x}) <$> isValidPort (confPort conf)

version1Parser :: YamlParser Configuration
version1Parser = objectParser "Configuration" configurationParser

version1 :: IO ()
version1 =
  T.putStrLn . prettySchema $
    explainParser version1Parser
