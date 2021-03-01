module Telegram
  ( User(..)
  , Chat(..)
  , PhotoSize(..)
  , Sticker(..)
  , Message(..)
  , Update(..)
  , TelegramResponse(..)
  , ClientEnv(..)
  , Client
  , runClient
  , getUpdates
  , GetUpdates(..)
  , sendMessage
  , SendMessage(..)
  ) where

import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Client
import Network.HTTP.Simple
import Telegram.Types

data ClientEnv =
  ClientEnv
    { clientEnv_host :: String
    , clientEnv_token :: String
    }
  deriving (Show)

type Client m a = ReaderT ClientEnv m a

runClient :: Client m a -> ClientEnv -> m a
runClient = runReaderT

buildApiPath :: (Monad m) => String -> Client m BS.ByteString
buildApiPath apiPath = do
  token <- asks clientEnv_token
  let path = mconcat ["/bot", token, apiPath]
  pure $ BS.pack path

defaultTelegramRequest :: (Monad m) => Client m Request
defaultTelegramRequest = asks $ parseRequest_ . clientEnv_host

buildRequest :: (Monad m) => String -> (Request -> Request) -> Client m Request
buildRequest path setter = do
  apiPath <- buildApiPath path
  setRequestPath apiPath . setter <$> defaultTelegramRequest

makeApiRequest :: (FromJSON a) => Request -> Client IO a
makeApiRequest request = do
  response <- liftIO $ httpJSON request
  let result = telegramResponse_result . getResponseBody $ response
  pure result

data GetUpdates =
  GetUpdates
    { getUpdates_offset :: Maybe Integer
    , getUpdates_timeout :: Maybe Integer
    , getUpdates_limit :: Maybe Integer
    }
  deriving (Show)

instance ToJSON GetUpdates where
  toJSON GetUpdates {..} =
    object
      [ "offset" .= getUpdates_offset
      , "timeout" .= getUpdates_timeout
      , "limit" .= getUpdates_limit
      ]

getUpdates' :: GetUpdates -> Client IO [Update]
getUpdates' options = do
  let setter = setRequestBodyJSON options
  request <- buildRequest "/getUpdates" setter
  makeApiRequest request

getUpdates :: GetUpdates -> ClientEnv -> IO [Update]
getUpdates options = runClient (getUpdates' options)

data SendMessage =
  SendMessage
    { sendMessage_chatId :: Integer
    , sendMessage_text :: String
    }
  deriving (Show)

instance ToJSON SendMessage where
  toJSON SendMessage {..} =
    object ["chat_id" .= sendMessage_chatId, "text" .= sendMessage_text]

sendMessage' :: SendMessage -> Client IO Message
sendMessage' options = do
  let setter = setRequestMethod "POST" . setRequestBodyJSON options
  request <- buildRequest "/sendMessage" setter
  makeApiRequest request

sendMessage :: SendMessage -> ClientEnv -> IO Message
sendMessage options = runClient (sendMessage' options)
