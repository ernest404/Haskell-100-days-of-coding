{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- import Data.Aeson

data Student = Student { firstName :: String,
                         lastName :: String,
                         birthDate :: Int,
                         specialization :: String,
                         studyYear :: Int,
                         averageGrade :: Float
                        } deriving (Show, Eq)

student_1 :: Student
student_1 = Student "Emily" "Brian" 1990 "Computer Science" 2 74

-- RecordConstructor{..} syntax

addD :: Student -> [String]
addD Student{firstName = fname, lastName = lname} = [fname ++"er", lname ++ "ed"]

addD' :: Student -> [String]
addD' Student{..} = [firstName ++ "er", lastName ++ "ed"]

-- For records with many fields, it can be tiresome to write out each field individually in a record pattern ... 
-- Record wildcard syntax permits a ".." in a record pattern, where each elided field f is replaced by the pattern f = f ... 
-- The expansion is purely syntactic, so the record wildcard expression refers to the nearest enclosing variables that are spelled the same as the omitted field names.

-- Using case expressions with 