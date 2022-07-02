import Data.List

votes :: [String]
votes = ["Biden", "Trump", "Biden", "Biden", "Biden",  "Trump", "Trump", "Trump", "Biden", "Biden", "Biden", "Trump", "Trump"]
-- function that counts values of one type in a list.
count :: Eq a => a -> [a] -> Int
count x = length.filter (==x)

-- function to get unique values of a list
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

-- [1,1,2,3,3,4,5,5]
-- (1: [2,3,3,4,5,5])
-- (1: (2 : [3,3,4,5,5]))
-- (1: (2: (3: [4,5,5])))

-- to get the results we use the two functions count and rmdups
-- In a list comprehension we take a list and remove duplicates so that we have unique values
-- Each element of unique value is sent to the operation side where we count that value in

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result
