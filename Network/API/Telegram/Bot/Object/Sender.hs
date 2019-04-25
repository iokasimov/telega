module Network.API.Telegram.Bot.Object.Sender (Sender (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), Object, withObject, (.:), (.:?))
import "aeson" Data.Aeson.Types (Parser)
import "base" Control.Applicative (Applicative ((<*>)))
import "base" Control.Monad (Monad ((>>=)))
import "base" Data.Bool (Bool (False), bool)
import "base" Data.Eq (Eq ((==)))
import "base" Data.Int (Int)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Maybe (Maybe)
import "base" Text.Show (Show)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Object.Name (Name, First, Last, Nick)
import Network.API.Telegram.Bot.Property.Accessible (Accessible (access))
import Network.API.Telegram.Bot.Property.Identifiable (Identifiable (Identificator, ident))

data Sender
	= Bot Int (Maybe (Nick Name)) (First Name) (Maybe (Last Name)) (Maybe Text)
	| Human Int (Maybe (Nick Name)) (First Name) (Maybe (Last Name)) (Maybe Text)
	deriving Show

instance Eq Sender where
	Bot i _ _ _ _ == Bot i' _ _ _ _ = i == i'
	Human i _ _ _ _ == Human i' _ _ _ _ = i == i'
	_ == _ = False

instance Identifiable Sender where
	type Identificator Sender = Int
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

type Whom = Int -> Maybe (Nick Name) -> First Name -> Maybe (Last Name) -> Maybe Text -> Sender

instance FromJSON Sender where
	parseJSON = withObject "Sender" $ \v -> v .: "is_bot"
		>>= bool (sender v Human) (sender v Bot) where

		sender :: Object -> Whom -> Parser Sender
		sender v f = f <$> v .: "id" <*> v .:? "username"
			<*> v .: "first_name" <*> v .:? "last_name"
			<*> v .:? "language_code"
