module Network.API.Telegram.Bot.Field.Name (Name (..), First (..), Last (..), Nick (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON)
	, ToJSON (toJSON), Value (String), withText)
import "base" Control.Applicative (pure)
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))

newtype Name = Name Text

instance Accessible Text Name where
	access f (Name txt) = (\txt' -> Name txt') <$> f txt

instance FromJSON Name where
	parseJSON = withText "Name" (pure . Name)

instance ToJSON Name where
	toJSON (Name txt) = String txt

data First a where First :: Name -> First Name

instance Accessible Text (First Name) where
	access f (First (Name txt)) = (\txt' -> First $ Name txt') <$> f txt

instance FromJSON (First Name) where
	parseJSON o = First <$> parseJSON o

instance ToJSON (First Name) where
	toJSON (First name) = toJSON name

data Last a where Last :: Name -> Last Name

instance Accessible Text (Last Name) where
	access f (Last (Name txt)) = (\txt' -> Last $ Name txt') <$> f txt

instance FromJSON (Last Name) where
	parseJSON o = Last <$> parseJSON o

instance ToJSON (Last Name) where
	toJSON (Last name) = toJSON name

data Nick a where Nick :: Name -> Nick Name

instance Accessible Text (Nick Name) where
	access f (Nick (Name txt)) = (\txt' -> Nick $ Name txt') <$> f txt

instance FromJSON (Nick Name) where
	parseJSON o = Nick <$> parseJSON o

instance ToJSON (Nick Name) where
	toJSON (Nick name) = toJSON name
