import Data.Word
import Data.Bits
import Data.List
import Data.Maybe
import System.Random


type Register = Word16

isPrime :: Register -> Bool
isPrime n = all (\x -> n `mod` x /= 0) [ 2 .. n `div` 2 ]

takeRandom :: [a] -> IO a
takeRandom [] = error "why would you do this"
takeRandom xs = (\r -> xs !! (r `mod` length xs)) <$> randomIO

multiplicativeInverse :: Register -> Register -> Register
multiplicativeInverse a m = fromIntegral $ fromJust $ find (\ai -> (ai * (fromIntegral a :: Int)) `mod` (fromIntegral m :: Int) == 1) [ 0 .. (fromIntegral m :: Int) ]

keys :: [Register]
keys = filter isPrime [ 2 ^ (s - 1) .. 2 ^ s ]
  where s = finiteBitSize (0 :: Register) `div` 2

data RSA_Public = RSA_Public {
  pbn :: Register,
  pbe :: Register
} deriving Show

data RSA_Private = RSA_Private {
  pvn :: Register,
  pvd :: Register
} deriving Show

rsa_encrypt :: RSA_Public -> Register -> Register
rsa_encrypt (RSA_Public n e) m = fromIntegral $ ((fromIntegral m :: Integer) ^ (fromIntegral e :: Integer)) `mod` (fromIntegral n :: Integer)

rsa_decrypt :: RSA_Private -> Register -> Register
rsa_decrypt (RSA_Private n d) c = fromIntegral $ ((fromIntegral c :: Integer) ^ (fromIntegral d :: Integer)) `mod` (fromIntegral n :: Integer)




factorize :: Register -> (Register, Register)
factorize n = let p = fromJust $ find (\x -> n `mod` x == 0) [ 2 .. n - 1 ] in
              (p, n `div` p)



rsa_generateFromPrimes :: Register -> Register -> (RSA_Public, RSA_Private)
rsa_generateFromPrimes p q = 
  let n = p * q in
  let ln = lcm (p - 1) (q - 1) in
  let (Just e) = find (\e -> gcd e ln == 1) [2 .. ln - 1] in
  let d = multiplicativeInverse e ln in
  
  (RSA_Public n e, RSA_Private n d)



main :: IO ()
main = do
  p <- takeRandom keys
  q <- takeRandom keys
  
  let (keyPub, _) = rsa_generateFromPrimes p q
  
  let message  = 0xabc
  let cipher   = rsa_encrypt keyPub message
  
  let (stolenP, stolenQ) = factorize (pbn keyPub)
  
  let (stolenKeyPub, stolenKeyPrv) = rsa_generateFromPrimes stolenP stolenQ
  let decipher = rsa_decrypt stolenKeyPrv cipher
  
  print [ message, cipher, decipher ]
  
  putStrLn "Hello, World!"
