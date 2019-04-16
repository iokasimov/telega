# 0.1.1
* Define `chat` lens for `Update` datatype
* Add string identificator to `Callback` datatype
* Define `Notification` datatype
* Define `Droppable` typeclass

# 0.1.2
* Define `Member` datatype
* Add new `Membership` constructor to `Update` datatype
* Define `Has` typeclass to get access objects within other objects
* Replace `chat` lens on `Access Update Chat` instance

# 0.1.3
* Rename `Has` typeclass to `Access` and swap parameters
* Rename `Member` datatype to `Moving`
* Define `Access Chat Callback` instance
* Define `Access From Callback` instance
* Define `Access Chat Moving` instance
* Define `Access Chat Message` instance
* Define `Access From Message` instance

# 0.1.4
* Define `Member` datatype for chat users
* No implicit `Prelude` anymore (NoImplicitPrelude)
* Move HTTP request function into `Internal` module
* Define `Endpoint` typeclass to replace `Capacity`
* Remove `Capacity` classes and theirs instances
* Define `Object` type family

# 0.1.5
* Remove `Internal` module, move its content to `Endpoint`
* Remove `Drop` phantom datatype, replace it with `Post`
* Use promoted constructors of `Capacity` instead of separated datatypes
* Use `MultiParamTypeClasses` instead of `FlexibleIntances` for `Endpoint`
* Use `UndecidableInstances` and `UndecidableSuperClasses` to use `Object` type family
* Replace `Access` module with `Accessible` `Property`
* Replace `Endpoint` module with `Persistable` `Property`

# 0.1.6
* Split `Message` on `Message` and `Content` datatypes
* Add `Forward` constructor to `Message` datatype
* Combine `Chat` and `From` objects into `Origin` datatype
* Put `Content`, `From` and `Origin` objects into `Message` submodule
* Put `Button` object into `Keyboard` submodule
* Put `Notification` object into `Callback` submodule
* Put `Keyboard` submodule into `Message` submodule
* Put `Callback`, `Message` and `Moving` modules into `Update` submodule
* Rename `From` to `Sender` and move it from `Message` and `Update` submodules

# 0.1.7
* Define `Identifiable` instance for `Callback` datatype
* Add `Reply` constructor to `Message` datatype
* Add `Attachment` constructor to `Content` datatype
* Define `File` for animations, audio, documents, photos, videos or voices
* Extract `Size` datatype into separated module within `Content` submodule
* Define `Location` and `Info` datatypes within `Content` submodule

# 0.1.8
* Add information about `Group` or `Supergroup` (id and title) into `Moving` datatype
* Add `Fetch` capacity, define `Persistable Fetch` for `Sender` to get info about bot
* Define `PL` newtype to avoid violating injectivity annotation of `Payload` type family
* Define `Messaging` datatype to separate direct, reply and forward `Message`
* Make first argument of `Payload` type family poly kinded (enable `PolyKinds`)
* Define `Persistable` instance for `Directly`, `Forwarding` and `Replying` `Message`

# 0.1.9
* Move `Messaging` datatype to `Persistable` module and make it over `Capacity` parameter
* Define `Persistable` instances for `Info` datatype (for direct and reply messages)
* Add ticks to all constructors of `Capacity` to surround them from two sides in promoted versions
* Define `Persistable` instance for `Member` (for kicking and unbanning only)
* Define `Powers` and `Restrictions` datatypes within created `Member` submodule

# 0.2.0
* Rename `Network.Telegram.API` to `Network.API.Telegram`
* Replace `PL` newtype with `Tagged` newtype from `tagged` package
* Replace `Message'` with `Way` and include it to `Capacity` with `Send` constructor
* Remove `user_id` field from `Contact` datatype
* Define `Persistable` instances for `Location` datatype
* Define `Inform` datatype and include it to `Capacity` with `Send` constructor
* Put `Payload` type family into `Persistable` type class

# 0.2.1
* Define `Poll` and his `Option` datatypes
* Put `Size` into `File`, `Option` into `Poll`, `Location` into `Info` created submodules
* Extract `Audio`,`Document`, `Video`, `Voice` from `File` datatype into modules
* Remove `Animation` construction from `File` datatype
* Rename `request` method to `persist` in `Persistable` type class
* Distribute `Persistable` instances on objects that belong to them
* Add `Polling` constructor to `Content` for `Poll` object
