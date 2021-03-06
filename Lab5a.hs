-- Exercise set 5a


module Lab5a where

-- help from Bryan, Callie

------------------------------------------------------------------------------
-- Ex 1: Define the type Vehicle that has four constructors: Bike,
-- Bus, Tram and Train.
--
-- The constructors don't need any fields.

data Vehicle = Bike | Bus | Tram | Train

------------------------------------------------------------------------------
-- Ex 2: Define the type BusTicket that can represent values like these:
--  - SingleTicket
--  - MonthlyTicket "January"
--  - MonthlyTicket "December"

data BusTicket = SingleTicket | MonthlyTicket String

------------------------------------------------------------------------------
-- Ex 3: define a datatype Person, which should contain the age (an
-- Int) and the name (a String) of a person.
--
-- Also define a Person value fred, and the functions getAge, getName,
-- setAge and setName (see below).

data Person = MkPerson {age :: Int, name :: String}
  deriving Show

-- fred is a person whose name is Fred and age is 90
fred :: Person
fred = MkPerson 90 "Fred"

-- getName returns the name of the person
getName :: Person -> String
getName (MkPerson age name) = name

-- getAge returns the age of the person
getAge :: Person -> Int
getAge (MkPerson age name) = age

-- setName takes a person and returns a new person with the name changed
setName :: String -> Person -> Person
setName name (MkPerson age a) = MkPerson age name

-- setAge does likewise for age
setAge :: Int -> Person -> Person
setAge age (MkPerson a name) =  MkPerson age name

------------------------------------------------------------------------------
-- Ex 4: define a datatype Position which contains two Int values, x
-- and y. Also define the functions below for operating on a Position.
--
-- Examples:
--   getY (up (up origin))    ==> 2
--   getX (up (right origin)) ==> 1

data Position = MkPosition Int Int
    deriving Show

-- origin is a Position value with x and y set to 0
origin :: Position
origin = MkPosition 0 0

-- getX returns the x of a Position
getX :: Position -> Int
getX (MkPosition x y) = x

-- getY returns the y of a position
getY :: Position -> Int
getY (MkPosition x y) = y

-- up increases the y value of a position by one
up :: Position -> Position
up (MkPosition x y) = MkPosition x (y+1)

-- right increases the x value of a position by one
right :: Position -> Position
right (MkPosition x y) = MkPosition (x+1) y


------------------------------------------------------------------------------
-- Ex 5: define a parameterized datatype OneOrTwo that contains one or
-- two values of the given type. The constructors should be called One and Two.
--
-- Examples:
--   One True         ::  OneOrTwo Bool
--   Two "cat" "dog"  ::  OneOrTwo String

data OneOrTwo k = One k | Two k k

------------------------------------------------------------------------------
-- Ex 6: define a recursive datatype KeyVals for storing a set of
-- key-value pairs. There should be two constructors: Empty and Pair.
--
-- Empty represents an empty collection. It should have no fields.
--
-- Pair should have three fields, one for the key, one for the value,
-- and one for the rest of the collection (of type KeyVals)
--
-- The KeyVals datatype is parameterized by the key type k and
-- the value type v.
--
-- For example:
--
--  Pair "cat" True (Pair "dog" False Empty)  ::  KeyVals String Bool
--
-- Also define the functions toList and fromList that convert between
-- KeyVals and lists of pairs.

data KeyVals k v = Empty | Pair k v (KeyVals k v)
  deriving Show

toList :: KeyVals k v -> [(k,v)]
toList Empty = []
toList (Pair a b x) = (a, b) : toList x

fromList :: [(k,v)] -> KeyVals k v
fromList [] = Empty
fromList ((k,v) : xs) = Pair k v (fromList xs) 

 