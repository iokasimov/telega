module Network.API.Telegram.Bot.Object.Update.Message.Content.Info.Contact (Contact (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import "base" Control.Applicative ((<*>))
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int)
import "base" Data.Maybe (Maybe)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Name (Name, First, Last)
import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))

data Contact = Contact (First Name) (Maybe (Last Name)) (Maybe Int) Text (Maybe Text) deriving Show

instance Accessible (First Name) Contact where
	access f (Contact fn ln uid pn vc) = (\fn' -> Contact fn' ln uid pn vc) <$> f fn

instance Accessible (Maybe (Last Name)) Contact where
	access f (Contact fn ln uid pn vc) = (\ln' -> Contact fn ln' uid pn vc) <$> f ln

instance FromJSON Contact where
	parseJSON = withObject "Contact" $ \i -> Contact
		<$> i .: "first_name" <*> i .:? "last_name"
		<*> i .:? "user_id" <*> i .: "phone_number"
		<*> i .:? "vcard"
