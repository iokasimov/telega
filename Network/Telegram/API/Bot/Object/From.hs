module Network.Telegram.API.Bot.Object.From
	(From (..), nickname, firstname, lastname) where

import "aeson" Data.Aeson (FromJSON (parseJSON), Object, withObject, (.:), (.:?))
import "aeson" Data.Aeson.Types (Parser)
import "base" Data.Bool (bool)
import "lens" Control.Lens (Lens')
import "text" Data.Text (Text)

data From
	= Bot Int (Maybe Text) Text (Maybe Text) (Maybe Text)
	| User Int (Maybe Text) Text (Maybe Text) (Maybe Text)
	deriving Show

type Whom = Int -> Maybe Text -> Text -> Maybe Text -> Maybe Text -> From

instance Eq From where
	Bot i _ _ _ _ == Bot i' _ _ _ _ = i == i'
	User i _ _ _ _ == User i' _ _ _ _ = i == i'
	_ == _ = False

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
