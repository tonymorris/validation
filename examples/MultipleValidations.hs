-- Example that shows how to validate a single value
-- against multiple validation functions.

-- Thanks to @purefn for the help on this!

import Control.Applicative
import Control.Lens
import Data.Validation

data VError = MustBePositive
            | MustBeEven
            | MustBeMultipleOf3
            deriving (Show)

-- ***** Base validation functions *****

-- Validates an Int is positive
intPositive :: Int -> AccValidation [VError] Int
intPositive x = if x > 0
                then _Success # x 
                else _Failure # [MustBePositive]

-- Validates an Int is even
intEven :: Int -> AccValidation [VError] Int
intEven x = if x `mod` 2 == 0
            then _Success # x 
            else _Failure # [MustBeEven]

-- Validates an Int is a multiple of 3
intMultiple3 :: Int -> AccValidation [VError] Int
intMultiple3 x = if x `mod` 3 == 0
                 then _Success # x 
                 else _Failure # [MustBeMultipleOf3]


-- ***** Combining validation functions *****

-- Validates an Int is positive and even
intPosEven :: Int -> AccValidation [VError] Int
intPosEven x = intPositive x *> intEven x

-- Validates an Int is positive, even, and a multiple of 3
intPosEvenThree :: Int -> AccValidation [VError] Int
intPosEvenThree x = intPositive x *> intEven x *> intMultiple3 x


-- ***** Example usage *****

successPositive = intPositive 5
-- AccSuccess 5

failurePositive = intPositive (-2)
-- AccFailure [MustBePositive]

successEven = intEven 2
-- AccSuccess 2

failureEven = intEven 3
-- AccFailure [MustBeEven]

successPosEven = intPosEven 2
--AccSuccess 2

failurePosEven1 = intPosEven (-2)
-- AccFailure [MustBePositive]

failurePosEven2 = intPosEven (-1)
-- AccFailure [MustBePositive,MustBeEven]

successPosEvenThree = intPosEvenThree 6
-- AccSuccess 6

failurePosEvenThree1 = intPosEvenThree 5
-- AccFailure [MustBeEven,MustBeMultipleOf3]

failurePosEvenThree2 = intPosEvenThree (-5)
-- AccFailure [MustBePositive,MustBeEven,MustBeMultipleOf3]

main :: IO ()
main = do
  putStrLn $ "intPositive 5:        " ++ show successPositive
  putStrLn $ "intPositive (-2):     " ++ show failurePositive
  putStrLn $ "intEven 2:            " ++ show successEven
  putStrLn $ "intEven 3:            " ++ show failureEven
  putStrLn $ "intPosEven 2:         " ++ show successPosEven
  putStrLn $ "intPosEven (-2):      " ++ show failurePosEven1
  putStrLn $ "intPosEven (-1):      " ++ show failurePosEven2
  putStrLn $ "intPosEvenThree 6:    " ++ show successPosEvenThree
  putStrLn $ "intPosEvenThree 5:    " ++ show failurePosEvenThree1
  putStrLn $ "intPosEvenThree (-5): " ++ show failurePosEvenThree2