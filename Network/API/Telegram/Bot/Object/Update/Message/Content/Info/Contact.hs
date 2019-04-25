module Network.API.Telegram.Bot.Object.Update.Message.Content.Info.Contact (Contact (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "base" Control.Applicative ((<*>))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Name (Name, First, Last)

data Contact = Contact (First Name) (Maybe (Last Name)) (Maybe Text) Text (Maybe Text) deriving Show

instance FromJSON Contact where
	parseJSON = withObject "Contact" $ \i -> Contact
		<$> i .: "first_name" <*> i .:? "last_name"
		<*> i .:? "user_id" <*> i .: "phone_number"
		<*> i .:? "vcard"
