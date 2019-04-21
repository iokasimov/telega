module Network.API.Telegram.Bot.Object.Update.Message.Content.Info (Info (..), module Exports) where

import Network.API.Telegram.Bot.Object.Update.Message.Content.Info.Contact as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.Info.Location as Exports
import Network.API.Telegram.Bot.Object.Update.Message.Content.Info.Venue as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "base" Control.Applicative ((<|>))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)

data Info
	= Point Location
	| User Contact
	| Meeting Venue
	deriving Show

instance FromJSON Info where
	parseJSON = withObject "Info" $ \v ->
		(User <$> v .: "contact") <|>
		(Meeting <$> v .: "venue") <|>
		(Point <$> v .: "location")
