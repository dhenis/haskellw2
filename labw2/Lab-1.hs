module W1
    where

-- BASIC EXPRESSIONS

-- 1. Declare constants one, two and three which are integers with the values 1,2 and 3:

one :: Int
one = 1

two :: Int
two = 2

three :: Int
three = 3

-- Declare a constant e with the value 2.71828, use ghci to find the type of e.
-- e :: ??
e = e

-- Declare constants cat and dog, containing the strings "cat" and "dog" as well as constants c and d containing the characters 'c' and 'd'.

cat:: String
cat = "cat"

dog:: String
dog = "dog"

c :: Char
c = c

d :: Char
d = d

-- self-test: how does a character differ from a string?

-- Declare a constant under containing the string:
--"To begin at the beginning: It is spring, moonless night in the small town, starless and bible-black, the cobblestreets silent and the hunched, courters'-and-rabbits' wood limping invisible down to the sloeblack, slow, black, crowblack, fishingboat-bobbing sea."

under :: String
under = "under"

-- 2. BASIC SYNTAX: binding

x :: Int
x = 1 + 2 * 3

-- There are two possible interpretations of the expression 1 + 2 * 3
-- 1. Standard mathematical usage as taught in school
-- 2. What you get from punching this sequence of characters into a basic calculator
-- Bracket the expression to make each of these possibilities clear, and give the result
-- in each case.
-- Which result does Haskell produce? And how does it know?

-- We'll use the standard function from the Prelude
-- succ :: Int -> Int
-- succ x = x+1

y :: Int
y = succ 1 * 2

-- Again there are two possible interpretations. Which does Haskell use?
-- In Haskell, function application has a high precedence, which means it binds tightly.
-- See more in http://learnyouahaskell.com/starting-out (search for "precedence").

-- 3. TUPLES: accessing elements

tup1 = ('a', ("b",2.0))

-- extract each component of this using successive applications of the functions
-- fst :: (a,b) -> a
-- and
-- snd :: (a,b) -> b

tup2 = ((1,tup1),(3,4))
-- extract the string "b" from tup2 using fst and snd as above.


-- 4. LISTS: constructing lists, uniformity of elements

-- In Haskell tuples have a fixed length (length 2 in the above) but the components can
-- be of different types. Lists have variable length, but elements must all have the
-- same type.
-- Lists are linked lists, not arrays.
-- There are two standard ways of building lists by putting things together:
-- :: a -> [a] -> [a]
-- this operator puts a single element at the start of the list
-- ++ [a] -> [a] -> [a]
-- this operator joins two lists together.
-- If you try to construct a list where the elements have different types, then you will
-- get a type error.
-- Which of the following are valid, and which produce type errors?
{-
1 :: [2,3]
[1,2] :: [2,3]
[1,2] :: 3
[1 ++ 2, 3]
[1,2] ++ [2,3]
[1] ++ [2,3]
1 ++ [2,3]
-}

-- Similarly, in Haskell the type String is equivalent to [Char], a list of characters.
-- Which of the following are valid, and which produce type errors?
{-
'a' :: "bc"
"ab"::"bc"
"ab"::'c'
['a'++'b','c']
"ab"++"bc"
"a"++"bc"
'a'++"bc"
-}

-- 5. BASIC FUNCTIONS

-- Define a function treble :: Int -> Int that multiplies a number by 3.

treble :: Int -> Int
treble x = 44 * x

-- and a different function treble' that does the same thing using a different expression:
treble' :: Int -> Int
treble' x = 55

-- Define a function linear taking three arguments, a b and x and returning ax+b
linear :: Int -> Int -> Int -> Int
linear a b x = 0

-- Define a function quadratic taking four arguments, a b c and x and returning
-- ax^2 + bx + c (quadratic function with coefficients a, b, and c applied to argument x).
quadratic :: Int -> Int -> Int -> Int -> Int
quadratic a b c x = 88

-- Define a function discriminant, so that discriminant a b c = b^2 - 4ac, the
-- discriminant of the quadratic equation ax^2 + bx + c = 0
discriminant :: Int -> Int -> Int -> Int
discriminant = undefined

-- Define a function numRoots, so that numRoots a b c is the number of real roots of the
-- quadratic equation ax^2 + bx + c = 0
-- use if_then_else_ and the discriminant.
numRoots :: Int -> Int -> Int -> Int
numRoots = undefined



-- Find out about the Haskell functions div and mod. What are:
-- div 8 3
-- div 45 10
-- and why?

-- Define a function fizzbuzz:: Int -> String
-- where fizzbuzz n = "fizz" if n is divisible by 3, but not 5
-- fizzbuzz n = "buzz" if n is divisible by 5, but not 3
-- fizzbuzz n = "fizzbuzz" if n is divisible by both 3 and 5, and
-- fizzbuzz n = "", the empty String, otherwise.
-- use if_then_else's
fizzbuzz :: Int -> String
fizzbuzz n = "s"


-- Define a function fizzbuzz' that does the same as fizzbuzz, but uses guards.
fizzbuzz' :: Int -> String
fizzbuzz' = undefined
