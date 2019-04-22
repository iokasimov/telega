# Inline Telegram Bot API library

Work in progress, there is an application for testing design of this library:
https://github.com/iokasimov/elections-bot

### Introduction

This library provides ORM-like toolkit to deal with methods of Telegram Bot API - types and classes were designed in terms of `Objects` and their `Properties`.

##### Objects description:

* `Update`: object that webhook consumes, can be either an incoming
`Message`, `Callback` query or `Moving` in/out some group chat.

* `Message`: every object contains `Origin` and `Content`, can be either `Direct`, `Forwarded` or `Replied`.

* `Content`: can be either `Command`, `Text`, `File`, `Poll`, `Contact`, `Location` or `Venue`.

* `File`: Can be either `Audio`, `Document`, `Photo`, `Video` or `Voice`.

* `Origin`: indicates from where is message from, contains `Sender`.

* `Callback`: this is what webhook consumes on pressing inline keyboard's button.

* `Moving`: indicates who joined or leaved some chat, contains `Sender`.

* `Sender`: can be either a `Bot` or a `Human`.

### Simple examples

#### Sending inline keyboard

```haskell
let text = "Just click any button..."
let button1 = Button "Press me!" $ Callback "1"
let button2 = Button "Or press me!" $ Callback "2"
let button3 = Button "No, press me!" $ Callback "3"
let keyboard = Inline [button1, button2, button3]
void . persist . Send chat_id $ text :&: keyboard
```

#### Handling pressing buttons with notification

```haskell
webhook (Query _ (Datatext cbq_id _ _ bn)) = do
	let text = "The button you pressed is: " <> bn
	persist $ Trigger @Notification cbq_id text
```

#### Deleting all incoming messages

```haskell
webhook (Incoming _ msg) = do
	let chat_id = ident $ access @Origin msg
	persist (Delete @Message chat_id $ ident msg)
```
