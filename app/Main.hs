module Main where

import App
import Config
import qualified Telegram

-- import Utils
main :: IO ()
main = do
  Config {..} <- readConfigFile "config.json"
  let logsFilePath = "Logs.log"
  writeFile logsFilePath ""
  let telegramEnv =
        Telegram.ClientEnv
          { Telegram.clientEnv_host = config_telegramHost
          , Telegram.clientEnv_token = config_botToken
          }
  let env =
        AppEnv
          { appEnv_pollingTimeout = config_pollingTimeout
          , appEnv_telegramEnv = telegramEnv
          , appEnv_logsFilePath = logsFilePath
          }
  let state = AppState {appState_offset = Nothing}
  -- message <- evalApp (sendMessage 468394736 "Это мой бот!") env state
  -- prettyPrint message
  _ <- evalApp app env state
  pure ()
