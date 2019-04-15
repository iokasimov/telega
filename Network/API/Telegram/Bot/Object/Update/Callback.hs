module Network.API.Telegram.Bot.Object.Update.Callback (Callback (..), module Exports) where

import Network.API.Telegram.Bot.Object.Update.Callback.Notification as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative (Applicative ((<*>)))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Update.Message (Message)

data Callback = Datatext Text Message Text deriving Show

instance FromJSON Callback where
	parseJSON = withObject "Callback" $ \v ->
		Datatext <$> v .: "id" <*> v .: "message" <*> v .: "data"
