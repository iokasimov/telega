module Network.API.Telegram.Bot.Object.Sender (Sender (..), ID (SNDR)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON)
	, Object, withObject, (.:), (.:?))
import "aeson" Data.Aeson.Types (Parser)
import "base" Control.Applicative (Applicative ((<*>)))
import "base" Control.Monad (Monad ((>>=)))
import "base" Data.Bool (Bool (False), bool)
import "base" Data.Eq (Eq ((==)))
import "base" Data.Int (Int)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe)

import Network.API.Telegram.Bot.Field (Language, Name, First, Last, Nick)
import Network.API.Telegram.Bot.Property (Accessible (access), Identifiable (Identificator, ident), ID)

data Sender
	= Bot (ID Sender) (Maybe (Nick Name)) (First Name) (Maybe (Last Name)) (Maybe Language)
	| Human (ID Sender) (Maybe (Nick Name)) (First Name) (Maybe (Last Name)) (Maybe Language)

instance Eq Sender where
	Bot i _ _ _ _ == Bot i' _ _ _ _ = i == i'
	Human i _ _ _ _ == Human i' _ _ _ _ = i == i'
	_ == _ = False

instance Identifiable Sender where
	type Identificator Sender = ID Sender
	ident (Bot i _ _ _ _) = i
	ident (Human i _ _ _ _) = i

instance Accessible (First Name) Sender where
	access f (Bot uid nn fn ln lng) = (\fn' -> Bot uid nn fn' ln lng) <$> f fn
	access f (Human uid nn fn ln lng) = (\fn' -> Human uid nn fn' ln lng) <$> f fn

instance Accessible (Maybe (Last Name)) Sender where
	access f (Bot uid nn fn ln lng) = (\ln' -> Bot uid nn fn ln' lng) <$> f ln
	access f (Human uid nn fn ln lng) = (\ln' -> Human uid nn fn ln' lng) <$> f ln

instance Accessible (Maybe (Nick Name)) Sender where
	access f (Bot uid nn fn ln lng) = (\nn' -> Bot uid nn' fn ln lng) <$> f nn
	access f (Human uid nn fn ln lng) = (\nn' -> Human uid nn' fn ln lng) <$> f nn

instance Accessible (Maybe Language) Sender where
	access f (Bot uid nn fn ln lng) = (\lng' -> Bot uid nn fn ln lng') <$> f lng
	access f (Human uid nn fn ln lng) = (\lng' -> Human uid nn fn ln lng') <$> f lng

type Whom = ID Sender -> Maybe (Nick Name) -> First Name
	-> Maybe (Last Name) -> Maybe Language -> Sender

instance FromJSON Sender where
	parseJSON = withObject "Sender" $ \v -> v .: "is_bot"
		>>= bool (sender v Human) (sender v Bot) where

		sender :: Object -> Whom -> Parser Sender
		sender v f = f <$> v .: "id" <*> v .:? "username"
			<*> v .: "first_name" <*> v .:? "last_name"
			<*> v .:? "language_code"

data instance ID Sender = SNDR Int

deriving instance Eq (ID Sender)

instance FromJSON (ID Sender) where parseJSON o = SNDR <$> parseJSON o
instance ToJSON (ID Sender) where toJSON (SNDR i) = toJSON i
