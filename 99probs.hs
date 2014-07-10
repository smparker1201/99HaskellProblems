import Data.List (sortBy) 
import Data.Ord (compare) 

myLast :: [Integer] -> [Char]
myLast [] = "No last element in empty list" 
myLast (x:[]) = show  x
myLast (_:x) =  myLast x

scnd_last :: [a] -> a
scnd_last (x:y:[]) = x
scnd_last (_:y) = scnd_last y

element_at ::  [a]->Int->a
element_at list i  = list!!(i-1)

myLen ::  [a]->Int
myLen [] = 0
myLen (_:xs) = 1+myLen xs

myReverse :: [a]->[a]
myReverse (x:[])=[x]
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) =>  [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs  = (head xs)==(last xs)  && (isPalindrome $ init $ tail xs)

firstLetter :: [Char] -> [Char]
firstLetter "" = "empty string" 
firstLetter all@(x:xs) = "first letter of " ++ all ++ " is " ++ [x]

multThree :: Int -> Int -> Int -> Int
multThree a b c = a*b*c

compress :: (Eq a) =>  [a] -> [a]
compress (x:xs@(y:ys))
	| x == y = compress xs
	| otherwise = x:compress xs
compress x = x 

listify :: [a] -> [[a]]
listify [] = []
listify (x:xs) = [x]:listify xs
	
pack :: (Eq a) => [a] -> [[a]]
pack list = packHelper (listify list) 
	where
		packHelper (x:xs@(y:ys)) 
			| head x == head y = packHelper ((x++y):ys) 
			| otherwise =  x:packHelper xs
		packHelper x = x

encode :: (Eq a) => [a] -> [(Int,a)]
encode list = map count (pack list) 
	where
		count lst = (length lst,head lst) 
flatten :: [[a]] -> [a]
flatten (x:xs@(y:ys)) = flatten ((x++y):ys)
flatten [x] = x

decode :: (Eq a) => [(Int,a)] -> [a]
decode list = flatten $map expand list
	where 
		expand lst = [(snd lst) | x<-[1..(fst lst)]]

duplicate ::  [a] -> [a]
duplicate [] = []
duplicate [x] = x:[x]
duplicate (x:xs) = x:x:duplicate xs

rep :: a -> Int -> [a]
rep x n = [x | y <- [1..n]]

repli :: [a] -> Int -> [a]
repli [] n = []
repli [x] n  = rep x n
repli (x:xs) n  = rep x n ++ repli xs n

dropEvery :: [a] -> Int -> [a]
dropEvery list n = helper list n n
	where
		helper [] _ _ = []
		helper (x:xs) n 1 = helper xs n n 
		helper (x:xs) n i = x:( helper xs n (i-1))

split :: [a] -> Int -> [[a]]
split list n = [take n list, drop n list] 

slice :: [a] -> Int -> Int -> [a]
slice list a b = take (b-a+1) $ drop (a-1) list 

rotate :: [a] -> Int -> [a]
rotate list n = drop n list ++ take n list  

remove_at :: [a] -> Int -> [a] 
remove_at list k = take (k-1) list ++ drop k list

insert_at :: [a] -> a -> Int -> [a]
insert_at list e k = take k list ++ [e] ++ drop k list

range :: Int -> Int -> [Int]
range a b = [a..b]


--TODO list stuff with random numbers

lfsort :: [[a]] -> [[a]]
lfsort list = sortBy (\first second -> compare (length first) (length second)) list

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True 
isPrime n = primeHelper n (quot (n-n`mod`2) 2-1)
	where
		primeHelper n 0 = True
		primeHelper n 1 = True 
		primeHelper n i 
			| n`mod`i==0 = False
			| otherwise = primeHelper n (i-2)


gcdiv :: Int -> Int -> Int 
gcdiv a 0 = a
gcdiv a b = gcdiv b (a`mod`b)

coprime :: Int -> Int -> Bool
coprime a b = gcdiv a b == 1

euler_phi :: Int -> Int 
euler_phi a = length$ filter (\x -> x==True) [coprime a x | x<-[1..(a-1)]]

primeFactors :: Int -> [Int]
primeFactors a =filter (\y -> isPrime y) (filter (\x -> a`mod`x==0) [x | x<-[1..(quot(a-a`mod`2) 2)]]) 




