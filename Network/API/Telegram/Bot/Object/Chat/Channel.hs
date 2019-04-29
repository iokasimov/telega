module Network.API.Telegram.Bot.Object.Chat.Channel (Channel (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative ((<*>))
import "base" Control.Monad ((>>=), fail)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int64)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Field (Title)

data Channel = Channel Title deriving Show

instance FromJSON Channel where
	parseJSON = withObject "Channel" $
		\chat -> Channel <$> chat .: "title"
