module App where

import Control.Monad.Except
import Control.Monad.RWS
import Data.Maybe

-- import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as Text.IO
import qualified Telegram
import Text.Pretty.Simple
import Utils

type Updates = [Telegram.Update]

type Logs = [String]

newtype AppState =
  AppState
    { appState_offset :: Maybe Integer
    }
  deriving (Show)

data AppEnv =
  AppEnv
    { appEnv_telegramEnv :: Telegram.ClientEnv
    , appEnv_pollingTimeout :: Integer
    , appEnv_logsFilePath :: FilePath
    }
  deriving (Show)

-- type AppState = Integer
type App m a = RWST AppEnv Logs AppState m a

runApp :: App m a -> AppEnv -> AppState -> m (a, AppState, Logs)
runApp = runRWST

execApp :: (Monad m) => App m a -> AppEnv -> AppState -> m (AppState, Logs)
execApp = execRWST

evalApp :: (Monad m) => App m a -> AppEnv -> AppState -> m (a, Logs)
evalApp = evalRWST

increaseOffset :: Updates -> App IO ()
increaseOffset [] = pure ()
increaseOffset updates =
  modify (\appState -> appState {appState_offset = newOffset})
  where
    newOffset = Just $ succ maxUpdateId
    maxUpdateId = maximum $ map Telegram.update_updateId updates

-- logOffset :: App IO ()
-- logOffset = do
--   offset <- gets appState_offset
--   tell ["Offset: " ++ show offset]
--
-- logUpdates :: Updates -> App IO ()
-- logUpdates updates = tell ["Got: " ++ (show . length $ updates) ++ " updates"]
requestUpdates :: App IO Updates
requestUpdates = do
  offset <- gets appState_offset
  pollingTimeout <- asks appEnv_pollingTimeout
  let options =
        Telegram.GetUpdates
          { getUpdates_offset = offset
          , getUpdates_timeout = Just pollingTimeout
          , getUpdates_limit = Nothing
          }
  telegramEnv <- asks appEnv_telegramEnv
  liftIO $ Telegram.getUpdates options telegramEnv

printUpdates :: Updates -> App IO ()
printUpdates = mapM_ liftPrint

liftPrint :: (MonadIO m, Show a) => a -> m ()
liftPrint = liftIO . prettyPrint

appLog :: (MonadIO m, Show a) => a -> App m ()
appLog logData = do
  logsFilePath <- asks appEnv_logsFilePath
  liftIO $ Text.IO.appendFile logsFilePath (pShowNoColor logData)
  liftIO $
    Text.IO.appendFile logsFilePath ("\n\n" <> "-----------------" <> "\n\n")

logUpdates :: Updates -> App IO ()
logUpdates [] = pure ()
logUpdates (update:updates) = appLog update >> logUpdates updates

replyToUpdate :: Telegram.Update -> App IO ()
replyToUpdate Telegram.Update {update_message = Just message} = do
  let chatId = Telegram.chat_id . Telegram.message_chat $ message
  let text = fromMaybe "Not implemented" . Telegram.message_text $ message
  telegramEnv <- asks appEnv_telegramEnv
  let options =
        Telegram.SendMessage
          {sendMessage_chatId = chatId, sendMessage_text = text}
  _ <- liftIO $ Telegram.sendMessage options telegramEnv
  liftPrint "Messsage sent"
replyToUpdate _ = liftPrint "Unsupported update"

sendMessage :: Integer -> String -> App IO Telegram.Message
sendMessage chatId message = do
  telegramEnv <- asks appEnv_telegramEnv
  let options =
        Telegram.SendMessage
          {sendMessage_chatId = chatId, sendMessage_text = message}
  liftIO $ Telegram.sendMessage options telegramEnv

app :: App IO ()
app = do
  offset <- gets appState_offset
  -- logOffset
  liftPrint $ "Offset: " ++ show offset
  updates <- requestUpdates
  -- logUpdates updates:
  increaseOffset updates
  mapM_ replyToUpdate updates
  liftPrint $ "Got " ++ show (length updates) ++ " updates"
  printUpdates updates
  logUpdates updates
  liftIO $ putChar '\n'
  app
