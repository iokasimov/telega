module Network.API.Telegram.Bot.Object.Sender (Sender (..), nickname, firstname, lastname) where

import "aeson" Data.Aeson (FromJSON (parseJSON), Object, object, withObject, (.:), (.:?))
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
import "tagged" Data.Tagged (Tagged)
import "text" Data.Text (Text)

import Network.API.Telegram.Bot.Property.Identifiable (Identifiable (Identificator, ident))
import Network.API.Telegram.Bot.Property.Persistable (Persistable (Payload, payload, endpoint), Capacity (Fetch))

data Sender
	= Bot Int (Maybe Text) Text (Maybe Text) (Maybe Text)
	| User Int (Maybe Text) Text (Maybe Text) (Maybe Text)
	deriving Show

instance Eq Sender where
	Bot i _ _ _ _ == Bot i' _ _ _ _ = i == i'
	User i _ _ _ _ == User i' _ _ _ _ = i == i'
	_ == _ = False

type Whom = Int -> Maybe Text -> Text -> Maybe Text -> Maybe Text -> Sender

instance FromJSON Sender where
	parseJSON = withObject "Sender" $ \v -> v .: "is_bot"
		>>= bool (sender v User) (sender v Bot) where

		sender :: Object -> Whom -> Parser Sender
		sender v f = f <$> v .: "id" <*> v .:? "username"
			<*> v .: "first_name" <*> v .:? "last_name"
			<*> v .:? "language_code"

instance Identifiable Sender where
	type instance Identificator Sender = Int
	ident (Bot i _ _ _ _) = i
	ident (User i _ _ _ _) = i

-- instance Persistable 'Fetch Sender where
-- 	type instance Payload 'Fetch Sender = Tagged ('Fetch Sender) ()
-- 	payload _ = null
-- 	endpoint _ = "getMe"

nickname :: Lens' Sender (Maybe Text)
nickname f (Bot uid nn fn ln lng) = (\nn' -> Bot uid nn' fn ln lng) <$> f nn
nickname f (User uid nn fn ln lng) = (\nn' -> User uid nn' fn ln lng) <$> f nn

firstname :: Lens' Sender Text
firstname f (Bot uid nn fn ln lng) = (\fn' -> Bot uid nn fn' ln lng) <$> f fn
firstname f (User uid nn fn ln lng) = (\fn' -> User uid nn fn' ln lng) <$> f fn

lastname :: Lens' Sender (Maybe Text)
lastname f (Bot uid nn fn ln lng) = (\ln' -> Bot uid nn fn ln' lng) <$> f ln
lastname f (User uid nn fn ln lng) = (\ln' -> User uid nn fn ln' lng) <$> f ln
