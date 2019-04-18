module Network.API.Telegram.Bot.Object.Update.Message.Content.Poll (Poll (..)) where

import Network.API.Telegram.Bot.Object.Update.Message.Content.Poll.Option as Exports

import "aeson" Data.Aeson (FromJSON (parseJSON), object, withObject, (.:), (.=))
import "aeson" Data.Aeson.Types (Object, Parser)
import "base" Control.Applicative ((<*>))
import "base" Control.Monad ((>>=))
import "base" Data.Bool (Bool (True, False), bool)
import "base" Data.Function (($))
import "base" Data.Functor ((<$>))
import "base" Data.Int (Int, Int64)
import "base" Text.Show (Show)
import "tagged" Data.Tagged (Tagged, untag)
import "text" Data.Text (Text)

-- import Network.API.Telegram.Bot.Property.Persistable
-- 	( Persistable (Payload, payload, endpoint), Capacity (Send)
-- 	, Inform (Notify, Silently), Way (Directly, Replying))

data Poll
	= Opened Text Text [Option]
	| Closed Text Text [Option]
	deriving Show

type State = Text -> Text -> [Option] -> Poll

instance FromJSON Poll where
	parseJSON = withObject "Pool" $ \v -> v .: "is_closed"
		>>= bool (state v Opened) (state v Closed) where

		state :: Object -> State -> Parser Poll
		state v f = f <$> v .: "id" <*> v .: "question" <*> v .: "options"

-- instance Persistable ('Send 'Notify 'Directly) Poll where
-- 	type instance Payload ('Send 'Notify 'Directly) Poll
-- 		= Tagged ('Send 'Notify 'Directly Poll) (Int64, Text, [Text])
-- 	payload (untag -> (chat_id, question, options)) =
-- 		object ["chat_id" .= chat_id, "question" .= question,
-- 			"options" .= options, "disable_notification" .= False]
-- 	endpoint _ = "sendPoll"
--
-- instance Persistable ('Send 'Silently 'Directly) Poll where
-- 	type instance Payload ('Send 'Silently 'Directly) Poll
-- 		= Tagged ('Send 'Silently 'Directly Poll) (Int64, Text, [Text])
-- 	payload (untag -> (chat_id, question, options)) =
-- 		object ["chat_id" .= chat_id, "question" .= question,
-- 			"options" .= options, "disable_notification" .= True]
-- 	endpoint _ = "sendPoll"
--
-- instance Persistable ('Send 'Notify 'Replying) Poll where
-- 	type instance Payload ('Send 'Notify 'Replying) Poll
-- 		= Tagged ('Send 'Notify 'Replying Poll) (Int64, Int, Text, [Text])
-- 	payload (untag -> (chat_id, reply_to_message_id, question, options)) =
-- 		object ["chat_id" .= chat_id, "question" .= question, "options" .= options,
-- 			 "reply_to_message_id" .= reply_to_message_id, "disable_notification" .= False]
-- 	endpoint _ = "sendPoll"
--
-- instance Persistable ('Send 'Silently 'Replying) Poll where
-- 	type instance Payload ('Send 'Silently 'Replying) Poll
-- 		= Tagged ('Send 'Silently 'Replying Poll) (Int64, Int, Text, [Text])
-- 	payload (untag -> (chat_id, reply_to_message_id, question, options)) =
-- 		object ["chat_id" .= chat_id, "question" .= question, "options" .= options,
-- 			"reply_to_message_id" .= reply_to_message_id, "disable_notification" .= True]
-- 	endpoint _ = "sendPoll"
