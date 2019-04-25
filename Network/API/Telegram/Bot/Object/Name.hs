module Network.API.Telegram.Bot.Object.Name
	(Name (..), First (..), Last (..), Nick (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON)
	, ToJSON (toJSON), Value (String), withText)
import "base" Control.Applicative (pure)
import "base" Data.Function ((.))
import "base" Data.Functor ((<$>))
import "base" Text.Show (Show)
import "text" Data.Text (Text)

newtype Name = Name Text deriving Show

instance FromJSON Name where
	parseJSON = withText "Name" (pure . Name)

instance ToJSON Name where
	toJSON (Name txt) = String txt

data First a where First :: Name -> First Name

deriving instance Show a => Show (First a)

instance FromJSON (First Name) where
	parseJSON o = First <$> parseJSON o

instance ToJSON (First Name) where
	toJSON (First name) = toJSON name

data Last a where Last :: Name -> Last Name

deriving instance Show a => Show (Last a)

instance FromJSON (Last Name) where
	parseJSON o = Last <$> parseJSON o

instance ToJSON (Last Name) where
	toJSON (Last name) = toJSON name

data Nick a where Nick :: Name -> Nick Name

deriving instance Show a => Show (Nick a)

instance FromJSON (Nick Name) where
	parseJSON o = Nick <$> parseJSON o

instance ToJSON (Nick Name) where
	toJSON (Nick name) = toJSON name
