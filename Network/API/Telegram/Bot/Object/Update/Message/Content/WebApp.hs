module Network.API.Telegram.Bot.Object.Update.Message.Content.WebApp where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withObject, (.:))
import "base" Control.Applicative ((<*>))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "text" Data.Text (Text)
import "base" Text.Show (Show)

import Network.API.Telegram.Bot.Field (URI)

data WebApp = WebApp URI
	deriving Show

instance FromJSON WebApp where
	parseJSON = withObject "WebApp" $ \v ->
		WebApp <$> v .: "url"

instance ToJSON WebApp where
	toJSON (WebApp uri) = toJSON uri