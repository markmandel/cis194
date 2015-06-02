-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import           HW01
import           Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

testReverseDigits :: (Integer, [Integer]) -> Bool
testReverseDigits (n, d) = toRevDigits n == d

testDigits :: (Integer, [Integer]) -> Bool
testDigits (n, d) = toDigits n == d

ex2Tests :: [Test]
ex2Tests = [ Test "reverse digits test " testReverseDigits
            [(1234, [4,3,2,1]), (0, []), (-17, [])],

            Test "digits test " testDigits
            [(1234, [1,2,3,4]), (0, []), (-17, [])]
          ]

-- Exercise 3 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (n, d) = doubleEveryOther n == d

ex3Tests :: [Test]
ex3Tests = [Test "double every other" testDoubleEveryOther
            [([4, 9, 5, 5], [4, 18, 5, 10]), ([0, 0], [0,0])]
            ]

-- Exercise 4 -----------------------------------------

testSumAllDigits :: ([Integer], Integer) -> Bool
testSumAllDigits (n, d) = sumDigits n == d

ex4Tests :: [Test]
ex4Tests = [Test "sum all digits in a series" testSumAllDigits
             [([10, 5, 18, 4], 19)]
            ]

-- Exercise 5 -----------------------------------------

testLuhn :: (Integer, Bool) -> Bool
testLuhn (n, d) = luhn n == d

ex5Tests :: [Test]
ex5Tests = [Test "test valid credit cards" testLuhn
            [(5594589764218858, True), (1234567898765432, False)]
            ]

-- Exercise 6 -----------------------------------------

testHanoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testHanoi (n, start, aux, end, result) = (hanoi n start aux end) == result

ex6Tests :: [Test]
ex6Tests = [ Test "Test tower of hanoi" testHanoi
              [(2, "a", "b", "c", [("a","c"), ("a","b"), ("c","b")])]
            ]

-- hanoi  ==

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
