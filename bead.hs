--Készítette: Soós Bálint HDX9MU
type Base   = Int
type BigNum = [Int]

toBigNum :: Base -> Integer -> BigNum
toBigNum b n
 | b<2 || n<0 = error ("toBigNum: improper arguments: " ++ show n ++ " " ++ show b)
 | n==1 || n==0 = [fromIntegral n]
 | otherwise = ((fromIntegral n) `mod` b) : toBigNum b (n `div` (toInteger b))

fromBigNum :: Base -> BigNum -> Integer
fromBigNum b bignum = sum[fromIntegral(bignum!!k) * ((fromIntegral b)^k) | k<-[0..length bignum-1]]

pad :: ([a] -> [a] -> [a]) -> a -> Int -> [a] -> [a]
pad f k h l = f [k | x<-[1..(h-length l)]] l

padLeft :: a -> Int -> [a] -> [a]
padLeft k h l = pad (\p -> (p ++)) k h l

padRight :: a -> Int -> [a] -> [a]
padRight k h l = pad (\p -> (++ p)) k h l

(<=>) :: BigNum -> BigNum -> (BigNum, BigNum)
(<=>) l1 l2
 | length l1 <1 && length l2 <1 = ([],[])
 | length l1 == length l2 = (l1,l2)
 | length l1 > length l2 = (l1,padRight 0 (length l1) l2)
 | length l1 < length l2 = (padRight 0 (length l2) l1,l2)

--addBigNums :: Base -> BigNum -> BigNum -> BigNum

sumBigNums :: Base -> [BigNum] -> BigNum
sumBigNums _ [] = []
sumBigNums _ [n] = n 
sumBigNums b (x1:x2:xs) = sumBigNums b ((addBigNums b x1 x2) : xs)

--subBigNums :: Base -> BigNum -> BigNum -> BigNum

diffBigNums :: Base -> [BigNum] -> BigNum
diffBigNums _ [] = []
diffBigNums _ [n] = n 
diffBigNums b (x1:x2:xs) = diffBigNums b ((subBigNums b x1 x2) : xs)

--logPowerBase :: Base -> Int -> (Int, Int)

powersOf :: Base -> Int -> [Int]
powersOf _ 0 = []
powersOf x y = l1 : powersOf x (y - l2)
  where (l1,l2) = (logPowerBase x y)