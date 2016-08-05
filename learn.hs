
myLast [] = error ""
myLast [x] = x
myLast (x:xs) = myLast xs

mySecondLast [] = error ""
mySecondLast [x] = error ""
mySecondLast [x,_] = x
mySecondLast (x:xs) = mySecondLast xs

elementAt [] i = error ""
elementAt (x:_) 0 = x
elementAt (_:xs) i = elementAt xs (i - 1)

myLength [] = 0
myLength x = myLengthHelper x 0

myLengthHelper [] i = i
myLengthHelper (_:xs) i = myLengthHelper xs (i + 1)

one _ = 1
oneList = map one
myLength' xs = foldl (+) (0) (oneList xs)

myReverseHelper :: [t] -> [t] -> [t]
myReverseHelper [] y = y
myReverseHelper [x] y = x:y
myReverseHelper (x:xs) y = myReverseHelper xs (x:y)

myReverse x = myReverseHelper x []

isPalindrome :: (Eq t) => [t] -> Bool
isPalindrome x = x == myReverse x

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

compress :: Eq a => [a] -> [a]
compress = foldr compress' []
    where
        compress' x [] = [x]
        compress' item acc = if item == head acc then item:tail acc else item:acc
