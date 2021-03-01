{-# LANGUAGE RecordWildCards #-}

module Telegram.Types
  ( User(..)
  , Chat(..)
  , PhotoSize(..)
  , Sticker(..)
  , Message(..)
  , Update(..)
  , TelegramResponse(..)
  ) where

import Data.Aeson

import GHC.Generics

data User =
  User
    { user_id :: Integer
    , user_isBot :: Bool
    , user_firstName :: String
    , user_lastName :: Maybe String
    , user_username :: Maybe String
    }
  deriving (Show, Generic)

instance ToJSON User

instance FromJSON User where
  parseJSON =
    withObject "User" $ \obj -> do
      user_id <- obj .: "id"
      user_isBot <- obj .: "is_bot"
      user_firstName <- obj .: "first_name"
      user_lastName <- obj .:? "last_name"
      user_username <- obj .:? "username"
      pure User {..}

data Chat =
  Chat
    { chat_id :: Integer
    , chat_type :: String
    , chat_username :: Maybe String
    , chat_firstName :: Maybe String
    , chat_lastName :: Maybe String
    }
  deriving (Show, Generic)

instance ToJSON Chat

instance FromJSON Chat where
  parseJSON =
    withObject "Chat" $ \obj -> do
      chat_id <- obj .: "id"
      chat_type <- obj .: "type"
      chat_username <- obj .:? "username"
      chat_firstName <- obj .:? "first_name"
      chat_lastName <- obj .:? "last_name"
      pure Chat {..}

data PhotoSize =
  PhotoSize
    { photoSize_fileId :: String
    , photoSize_fileUniqueId :: String
    , photoSize_width :: Integer
    , photoSize_height :: Integer
    , photoSize_fileSize :: Maybe Integer
    }
  deriving (Show, Generic)

instance ToJSON PhotoSize

instance FromJSON PhotoSize where
  parseJSON =
    withObject "PhotoSize" $ \obj -> do
      photoSize_fileId <- obj .: "file_id"
      photoSize_fileUniqueId <- obj .: "file_unique_id"
      photoSize_width <- obj .: "width"
      photoSize_height <- obj .: "height"
      photoSize_fileSize <- obj .:? "file_size"
      pure PhotoSize {..}

data Sticker =
  Sticker
    { sticker_fileId :: String
    , sticker_fileUniqueId :: String
    , sticker_width :: Integer
    , sticker_height :: Integer
    , sticker_isAnimated :: Bool
    , sticker_thumb :: Maybe PhotoSize
    , sticker_emoji :: Maybe String
    , sticker_setName :: Maybe String
    , sticker_fileSize :: Maybe Integer
    }
  deriving (Show, Generic)

instance ToJSON Sticker

instance FromJSON Sticker where
  parseJSON =
    withObject "Sticker" $ \obj -> do
      sticker_fileId <- obj .: "file_id"
      sticker_fileUniqueId <- obj .: "file_unique_id"
      sticker_width <- obj .: "width"
      sticker_height <- obj .: "height"
      sticker_isAnimated <- obj .: "is_animated"
      sticker_thumb <- obj .:? "thumb"
      sticker_emoji <- obj .:? "emoji"
      sticker_setName <- obj .:? "set_name"
      sticker_fileSize <- obj .:? "file_size"
      pure Sticker {..}

data Message =
  Message
    { message_messageId :: Integer
    , message_from :: Maybe User
    , message_date :: Integer
    , message_chat :: Chat
    , message_text :: Maybe String
    , message_photo :: Maybe [PhotoSize]
    , message_sticker :: Maybe Sticker
    }
  deriving (Show, Generic)

instance FromJSON Message where
  parseJSON =
    withObject "Message" $ \obj -> do
      message_messageId <- obj .: "message_id"
      message_from <- obj .:? "from"
      message_date <- obj .: "date"
      message_chat <- obj .: "chat"
      message_text <- obj .:? "text"
      message_photo <- obj .:? "photo"
      message_sticker <- obj .:? "sticker"
      pure Message {..}

instance ToJSON Message

data Update =
  Update
    { update_updateId :: Integer
    , update_message :: Maybe Message
    }
  deriving (Show, Generic)

instance ToJSON Update

instance FromJSON Update where
  parseJSON =
    withObject "Update" $ \obj -> do
      update_updateId <- obj .: "update_id"
      update_message <- obj .:? "message"
      pure Update {..}

data TelegramResponse a =
  TelegramResponse
    { telegramResponse_ok :: Bool
    , telegramResponse_description :: Maybe String
    , telegramResponse_result :: a
    }
  deriving (Show)

instance (FromJSON a) => FromJSON (TelegramResponse a) where
  parseJSON =
    withObject "TelegramResponse" $ \obj -> do
      telegramResponse_ok <- obj .: "ok"
      telegramResponse_description <- obj .:? "description"
      telegramResponse_result <- obj .: "result"
      pure TelegramResponse {..}
