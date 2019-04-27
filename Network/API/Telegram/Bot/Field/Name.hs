module Network.API.Telegram.Bot.Field.Name (Name (..), First (..), Last (..), Nick (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON)
	, ToJSON (toJSON), Value (String), withText)
import "base" Control.Applicative (pure)
import "base" Data.Function ((.), ($))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))

newtype Name = Name Text deriving Show

instance Accessible Text Name where
	access f (Name txt) = (\txt' -> Name txt') <$> f txt

instance FromJSON Name where
	parseJSON = withText "Name" (pure . Name)

instance ToJSON Name where
	toJSON (Name txt) = String txt

data First a where First :: Name -> First Name

deriving instance Show a => Show (First a)

instance Accessible Text (First a) where
	access f (First (Name txt)) = (\txt' -> First $ Name txt') <$> f txt

instance FromJSON (First Name) where
	parseJSON o = First <$> parseJSON o

instance ToJSON (First Name) where
	toJSON (First name) = toJSON name

data Last a where Last :: Name -> Last Name

deriving instance Show a => Show (Last a)

instance Accessible Text (Last a) where
	access f (Last (Name txt)) = (\txt' -> Last $ Name txt') <$> f txt

instance FromJSON (Last Name) where
	parseJSON o = Last <$> parseJSON o

instance ToJSON (Last Name) where
	toJSON (Last name) = toJSON name

data Nick a where Nick :: Name -> Nick Name

deriving instance Show a => Show (Nick a)

instance Accessible Text (Nick a) where
	access f (Nick (Name txt)) = (\txt' -> Nick $ Name txt') <$> f txt

instance FromJSON (Nick Name) where
	parseJSON o = Nick <$> parseJSON o

instance ToJSON (Nick Name) where
	toJSON (Nick name) = toJSON name
