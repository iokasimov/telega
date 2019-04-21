module Network.API.Telegram.Bot.Object.Sender (Sender (..), nickname, firstname, lastname) where

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
import "lens" Control.Lens (Lens')
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Property.Identifiable (Identifiable (Identificator, ident))

data Sender
	= Bot Int (Maybe Text) Text (Maybe Text) (Maybe Text)
	| Human Int (Maybe Text) Text (Maybe Text) (Maybe Text)
	deriving Show

instance Eq Sender where
	Bot i _ _ _ _ == Bot i' _ _ _ _ = i == i'
	Human i _ _ _ _ == Human i' _ _ _ _ = i == i'
	_ == _ = False

instance Identifiable Sender where
	type Identificator Sender = Int
	ident (Bot i _ _ _ _) = i
	ident (Human i _ _ _ _) = i

type Whom = Int -> Maybe Text -> Text -> Maybe Text -> Maybe Text -> Sender

instance FromJSON Sender where
	parseJSON = withObject "Sender" $ \v -> v .: "is_bot"
		>>= bool (sender v Human) (sender v Bot) where

		sender :: Object -> Whom -> Parser Sender
		sender v f = f <$> v .: "id" <*> v .:? "username"
			<*> v .: "first_name" <*> v .:? "last_name"
			<*> v .:? "language_code"

nickname :: Lens' Sender (Maybe Text)
nickname f (Bot uid nn fn ln lng) = (\nn' -> Bot uid nn' fn ln lng) <$> f nn
nickname f (Human uid nn fn ln lng) = (\nn' -> Human uid nn' fn ln lng) <$> f nn

firstname :: Lens' Sender Text
firstname f (Bot uid nn fn ln lng) = (\fn' -> Bot uid nn fn' ln lng) <$> f fn
firstname f (Human uid nn fn ln lng) = (\fn' -> Human uid nn fn' ln lng) <$> f fn

lastname :: Lens' Sender (Maybe Text)
lastname f (Bot uid nn fn ln lng) = (\ln' -> Bot uid nn fn ln' lng) <$> f ln
lastname f (Human uid nn fn ln lng) = (\ln' -> Human uid nn fn ln' lng) <$> f ln
