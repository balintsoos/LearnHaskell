type Base   = Int
type BigNum = [Int]
type Granularity = Int

toBigNum :: Base -> Integer -> BigNum
toBigNum b n
 | b<2 || n<0 = error ("toBigNum: improper arguments: " ++ show n ++ " " ++ show b)
 | n == 0 || n < (fromIntegral b) = [fromIntegral n]
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

addBigNums :: Base -> BigNum -> BigNum -> BigNum
segedadd b [] [] z = []
segedadd b (x:[]) (y:[]) z
 | (x+y+z) > b = ((x+y+z) `mod` b) : [(x+y+z) `div` b]
 | otherwise = [(x+y+z) `mod` b]

segedadd b (x:xs) (y:ys) z = ((x+y+z) `mod` b) : segedadd b xs ys ((x+y+z) `div` b)

addBigNums b x y = segedadd b (fst (x <=> y)) (snd (x <=> y)) 0

sumBigNums :: Base -> [BigNum] -> BigNum
sumBigNums _ [] = []
sumBigNums _ [n] = n 
sumBigNums b (x1:x2:xs) = sumBigNums b ((addBigNums b x1 x2) : xs)

subBigNums :: Base -> BigNum -> BigNum -> BigNum
segedsub b (x:[]) (y:[]) z
 | (x-y+z) < 0 = (b+(x-y+z)):[(-1)]
 | otherwise = [(x-y+z)]
segedsub b (x:xs) (y:ys) z
 | (x-y+z) < 0 = b+(x-y+z) : segedsub b xs ys (-1)
 | otherwise = (x-y+z) : segedsub b xs ys 0
subBigNums b x y = segedsub b (fst (x <=> y)) (snd (x <=> y)) 0

diffBigNums :: Base -> [BigNum] -> BigNum
diffBigNums _ [] = []
diffBigNums _ [n] = n 
diffBigNums b (x1:x2:xs) = diffBigNums b ((subBigNums b x1 x2) : xs)

logPowerBase :: Base -> Int -> (Int, Int)
segedlog b i x
 | i < (fromIntegral b)^x = (x-1, b^(x-1))
 | otherwise = segedlog b i (x+1)
logPowerBase b i
 | b <= 1 = error ("logPowerBase: improper arguments")
 | otherwise = segedlog (fromIntegral b) (fromIntegral i) 1

powersOf :: Base -> Int -> [Int]
powersOf _ 0 = []
powersOf x y = l1 : powersOf x (y - l2)
 where (l1,l2) = (logPowerBase x y)

multBigNum :: Base -> Int -> BigNum -> BigNum
multBigNum b s x = sumBigNums b [padLeft 0 (((powersOf b s)!!i)+(length x)) x | i<-[0..length(powersOf b s)-1]]

multBigNums :: Base -> BigNum -> BigNum -> BigNum
multBigNums b x y = sumBigNums b [padLeft 0 (length (multBigNum b (x!!i) y)+i) (multBigNum b (x!!i) y) | i<-[0..(length x)-1]]

split = \n -> \xs -> (take n xs, drop n xs)

karatsuba :: Base -> Granularity -> BigNum -> BigNum -> BigNum
karatsuba b g x y
 | length x <= g || length y <= g = multBigNums b x y
 | otherwise = end
 where ((l1,h1),(l2,h2)) = (split (ceiling(fromIntegral(length(fst (x <=> y)))/2)) (fst (x <=> y)), split (ceiling(fromIntegral(length(snd (x <=> y)))/2)) (snd (x <=> y)));
       z0 = multBigNums b l1 l2;
       z1 = multBigNums b (addBigNums b l1 h1) (addBigNums b l2 h2);
       z2 = multBigNums b h1 h2;
       m2 = length l1;
       end = addBigNums b (padLeft 0 (2*m2+length z2) z2) (addBigNums b (padLeft 0 (m2+(length (subBigNums b (subBigNums b z1 z2) z0))) (subBigNums b (subBigNums b z1 z2) z0)) z0)