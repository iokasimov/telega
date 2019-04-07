module Network.Telegram.API.Bot.Capacity.Purgeable (Purgeable (..), Marking) where

import "aeson" Data.Aeson (Value, decode)
import "base" Control.Exception (try)
import "base" Control.Monad (Monad ((>>=)), join, void)

import "base" Data.Maybe (fromJust)
import "base" Data.String (String)
import "base" Data.Bool (Bool (True, False))
import "base" Data.Eq (Eq)
import "base" Data.Either (Either)
import "base" Data.Function (flip, (.), ($))
import "base" Data.Functor (Functor (fmap), (<$>))
import "base" Data.Semigroup (Semigroup ((<>)))
import "base" Data.Maybe (Maybe (Just, Nothing))
import "base" Data.String (String)
import "base" Data.Tuple (snd)
import "base" System.IO (IO)
import "base" Text.Show (Show)
import "text" Data.Text (Text)
import "http-client" Network.HTTP.Client (Response (responseBody))
import "text" Data.Text (unpack)
import "transformers" Control.Monad.Trans.Class (lift)
import "transformers" Control.Monad.Trans.Except (ExceptT (ExceptT))
import "transformers" Control.Monad.Trans.Reader (ask)

import qualified "wreq" Network.Wreq.Session as Wreq (post)

import Network.Telegram.API.Bot.Core (Telegram, Token (Token), Ok, result)

type family Marking a = r | r -> a

class Purgeable a where
	{-# MINIMAL marking_value, purge_endpoint #-}
	marking_value :: Marking a -> Value
	purge_endpoint :: Marking a -> String

	purge :: Marking a -> Telegram e ()
	purge x = snd <$> ask >>= \(session, Token token) -> void . lift . ExceptT . try
		. fmap (fromJust . join . fmap result . decode @(Ok ()) . responseBody)
			. flip (Wreq.post session) (marking_value x) $
				"https://api.telegram.org/" <> unpack token <> "/" <> purge_endpoint x
