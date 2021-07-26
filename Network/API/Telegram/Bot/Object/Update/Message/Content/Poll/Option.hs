module Network.API.Telegram.Bot.Object.Update.Message.Content.Poll.Option (Option (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withObject, (.:))
import "base" Control.Applicative ((<*>))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Field (Title)

data Option = Option Title Int
	deriving Show

instance FromJSON Option where
	parseJSON = withObject "Option" $ \v -> Option
		<$> v .: "text" <*> v .: "voter_count"

instance ToJSON Option where
	toJSON (Option title _) = toJSON title
