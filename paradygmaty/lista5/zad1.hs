import System.CPUTime
import Text.Printf

measureTime :: IO a -> IO (a, Double)
measureTime action = do
    startTime <- getCPUTime
    result <- action
    endTime <- getCPUTime
    let elapsedTime = fromIntegral (endTime - startTime) / (10^12)
    return (result, elapsedTime)

--binomial
binomial n 0 = 1
binomial n k = 
    if n == k then 1 else binomial (n-1) (k-1) + binomial (n-1) k

--binomial2
pascalInfinite = [1] : map (\l -> zipWith (+) (l ++ [0]) (0:l)) pascalInfinite
binomial2 n k = (pascalInfinite !! n) !! k


--mergesort 
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort firstHalf) (mergesort secondHalf)
  where
    (firstHalf, secondHalf) = splitAt (length xs `div` 2) xs


--de 
extendedGcd a 0 = (a, 1, 0)
extendedGcd a b = (d, y, x - (a `div` b) * y)
  where
    (d, x, y) = extendedGcd b (a `mod` b)

de a b = extendedGcd a b

-- prime_factors
primeFactors n = primeFactors' n 2 []
  where
    primeFactors' 1 _ acc = acc
    primeFactors' n d acc
      | n `mod` d == 0 = primeFactors' (n `div` d) d (d : acc)
      | otherwise      = primeFactors' n (d + 1) acc


--totient
totient n = length [x | x <- [1..n], gcd x n == 1]

-- phi(n) = (p1^(k1 -1)) * (p1 - 1) * (p2^(k2 -1)) * (p2 - 1) *    ... * (pr^(kr -1)) * (pr - 1)

totient2 n = totient2' 1 (primeFactors n)
  where
    totient2' acc [] = acc
    totient2' acc [x] = acc * (x - 1)
    totient2' acc (x:y:xs)
      | x == y    = totient2' (acc * x) (y:xs)
      | otherwise = totient2' (acc * (x - 1)) (y:xs)


--primes
primes :: Int -> [Int]
primes n = primes' [2..n] 0
  where
    primes' [] _ = []
    primes' numbers it
      | it * it == n = numbers
      | otherwise    = head numbers : primes' [x | x <- tail numbers, x `mod` head numbers /= 0] (it + 1)

main :: IO ()
main = do
  let binomialResult = binomial 5 2
  (binomialResult, binomialTime) <- measureTime $ return (binomialResult)
  putStrLn $ "binomial 5 2: " ++ show binomialResult ++ " (took " ++ printf "%.6f" binomialTime ++ " seconds)"

  let binomial2Result = binomial2 5 2
  (binomial2Result, binomial2Time) <- measureTime $ return (binomial2Result)
  putStrLn $ "binomial2 5 2: " ++ show binomial2Result ++ " (took " ++ printf "%.6f" binomial2Time ++ " seconds)"

  putStrLn ("mergesort [5,1,9,3,7,6,2,8,4]: " ++ show (mergesort [5,1,9,3,7,6,2,8,4]))
  putStrLn ("de 15 4: " ++ show(de 15 4))
  putStrLn ("primeFactors 21: " ++ show(primeFactors 21))

  let totientResult = totient 21
  (totientResult, totientTime) <- measureTime $ return (totientResult)
  putStrLn $ "totient 21: " ++ show totientResult ++ " (took " ++ printf "%.6f" totientTime ++ " seconds)"

  let totient2Result = totient2 21
  (totient2Result, totient2Time) <- measureTime $ return (totient2Result)
  putStrLn $ "totient2 21: " ++ show totient2Result ++ " (took " ++ printf "%.6f" totient2Time ++ " seconds)"

  putStrLn ("primes 21: " ++ show (primes 21))
