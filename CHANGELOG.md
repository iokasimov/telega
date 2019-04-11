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
