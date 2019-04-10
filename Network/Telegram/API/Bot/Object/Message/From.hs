module Network.Telegram.API.Bot.Object.Message.From
	(From (..), nickname, firstname, lastname) where

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

data From
	= Bot Int (Maybe Text) Text (Maybe Text) (Maybe Text)
	| User Int (Maybe Text) Text (Maybe Text) (Maybe Text)
	deriving Show

instance Eq From where
	Bot i _ _ _ _ == Bot i' _ _ _ _ = i == i'
	User i _ _ _ _ == User i' _ _ _ _ = i == i'
	_ == _ = False

type Whom = Int -> Maybe Text -> Text -> Maybe Text -> Maybe Text -> From

instance FromJSON From where
	parseJSON = withObject "From" $ \v -> v .: "is_bot"
		>>= bool (from v User) (from v Bot) where

		from :: Object -> Whom -> Parser From
		from v f = f <$> v .: "id"
			<*> v .:? "username"
			<*> v .: "first_name"
			<*> v .:? "last_name"
			<*> v .:? "language_code"

nickname :: Lens' From (Maybe Text)
nickname f (Bot uid nn fn ln lng) = (\nn' -> Bot uid nn' fn ln lng) <$> f nn
nickname f (User uid nn fn ln lng) = (\nn' -> User uid nn' fn ln lng) <$> f nn

firstname :: Lens' From Text
firstname f (Bot uid nn fn ln lng) = (\fn' -> Bot uid nn fn' ln lng) <$> f fn
firstname f (User uid nn fn ln lng) = (\fn' -> User uid nn fn' ln lng) <$> f fn

lastname :: Lens' From (Maybe Text)
lastname f (Bot uid nn fn ln lng) = (\ln' -> Bot uid nn fn ln' lng) <$> f ln
lastname f (User uid nn fn ln lng) = (\ln' -> User uid nn fn ln' lng) <$> f ln
