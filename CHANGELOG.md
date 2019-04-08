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
* Remove `Object` type family
