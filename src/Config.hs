{-# LANGUAGE RecordWildCards #-}

module Config where

import Data.Aeson

data Config =
  Config
    { config_pollingTimeout :: Integer
    , config_botToken :: String
    , config_telegramHost :: String
    -- config_telegramBotHost :: String
    }
  deriving (Show)

instance FromJSON Config where
  parseJSON =
    withObject "Config" $ \obj -> do
      config_pollingTimeout <- obj .: "pollingTimeout"
      config_botToken <- obj .: "botToken"
      config_telegramHost <- obj .: "telegramHost"
    -- let config_telegramBotHost = mconcat [config_telegramHost, "/bot", config_botToken]
      pure $ Config {..}

-- instance ToJSON Config where
--  toJSON Config {..} = object ["timeout" .= timeout, "botToken" .= botToken]
readConfigFile :: FilePath -> IO Config
readConfigFile filePath = do
  config <- eitherDecodeFileStrict' filePath
  pure $!
    case config of
      Left message -> error message
      Right value -> value
