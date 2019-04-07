module Network.Telegram.API.Bot.Object.Member (Member (..)) where

import "aeson" Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import "text" Data.Text (Text)
import "time" Data.Time.Clock.POSIX (POSIXTime)

import Network.Telegram.API.Bot.Object.From (From)

data Member
	= Creator From
	| Administrator From
	| Member From
	| Restricted From POSIXTime
	-- | Left From
	| Kicked From POSIXTime
	deriving Show

instance FromJSON Member where
	parseJSON = withObject "Member" $ \v -> v .: "status" >>= \case
		("creator" :: Text) -> Creator <$> v .: "user"
		("administrator" :: Text) -> Administrator <$> v .: "user"
		("member" :: Text) -> Member <$> v .: "user"
		("restricted" :: Text) -> Restricted <$> v .: "user" <*> v .: "until_date"
		-- ("left" :: Text) -> Left <$> v .: "user"
		("kicked" :: Text) -> Kicked <$> v .: "user" <*> v.: "until_date"
		_ -> fail "Status of chat member is not defined"
