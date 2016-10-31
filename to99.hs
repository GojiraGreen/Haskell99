
-- problem 1

myLast [] = error "No end for empty list"
myLast (x : xs) 
    | xs == [] = x
    | otherwise = myLast xs

myLast' [] = error "no end for empty list"
myLast' [x] = x
myLast' (_:xs) = myLast xs

myLast'' = head . reverse

-- problem 2

myButLast [] = error "No but end for empty list"
myButLast [x] = error "No but end for one element list"
myButLast (x : xs)
    | tail xs == [] = x
    | otherwise = myButLast xs

myButLast' = head . tail . reverse

-- problem 3

elementAt [] _ = error "Index out of bound"
elementAt (x : xs) n
    | n <= 0 = error "Index out of bound"
    | n == 1 = x
    | otherwise = elementAt xs (n-1)

-- problem 4

myLenght [] = 0
myLenght (x : xs) = 1 + myLenght xs

-- problem 5

myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- problem 6

isPalindrome x = x == myReverse x

-- problem 7

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x : xs)) = (flatten x) ++ (flatten (List xs))

-- problem 8

consists [] z = False
consists (x:xs) z
    | x == z = True
    | otherwise = consists xs z

compress :: Eq a => [a] -> [a]
compress x = compress' [] x

compress' :: Eq a => [a] -> [a] -> [a]
compress' r [] = r
compress' z (x : xs)
    | z `consists` x = compress' z xs
    | otherwise = compress' (z ++ [x]) xs


compress'' (x:ys@(y:_))
        | x == y = compress'' ys
        | otherwise = x : compress'' ys
compress'' ys = ys

-- problem 9

pack :: Eq a => [a] -> [[a]] 
pack [] = []
pack x = packUnit' [] x

packUnit' :: Eq a => [[a]] -> [a] -> [[a]]
packUnit' v [] = v
packUnit' v (x:xs) = v ++ compressUnit [x] xs

compressUnit :: Eq a => [a] -> [a] -> [[a]]
compressUnit v [] = [v]
compressUnit v (x:xs)
    | x == head v = compressUnit (v ++ [x]) xs
    | otherwise = packUnit' [v] ([x] ++ xs)

-- problem 10

encode x =  encode' (pack x)
encode' x = map (\ n -> (length n, head n)) x

-- problem 11

data MyEncode a = Single a | Multiple Int a
    deriving (Show)

myFilter :: Eq a => [a] -> MyEncode a
myFilter x
    | length x == 1 = Single (head x)
    | otherwise = Multiple (length x) (head x)

encodeMdified ::  Eq a => [a] -> [MyEncode a]
encodeMdified x =  encodeMdified' (pack x)

encodeMdified' ::  Eq a => [[a]] -> [MyEncode a]
encodeMdified' x = map (\ n -> myFilter n) x