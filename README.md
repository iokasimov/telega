# Inline Telegram Bot API library

Work in progress, there is an application for testing design of this library:
https://github.com/iokasimov/elections-bot

### Introduction

This library provides ORM-like toolkit to deal with methods of Telegram Bot API - types and classes were designed in terms of `Objects` and their `Properties`.

### Examples

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
webhook (Query _ (Datatext cbq_id (Direct _ origin _) bn)) = do
	let text = "The button you pressed is: " <> bn
	persist $ Trigger @Notification cbq_id text
```

#### Deleting all incoming messages

```haskell
webhook (Incoming _ msg) = do
	let chat_id = ident $ access @Origin msg
	persist (Delete @Message chat_id $ ident msg)
```
