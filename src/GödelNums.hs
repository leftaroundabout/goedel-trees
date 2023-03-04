-- |
-- Module      : GödelNums
-- Copyright   : (c) Justus Sagemüller 2023
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 

module GödelNums where

data Nat = Zero
         | Gödel Nat Nat
            -- ^ @Gödel a b@ represents @(a+b+1)*(a+b)/2 + b + 1@.
  deriving (Show, Eq)

instance Num Nat where
  fromInteger n | n<0 = error "Negative"
  fromInteger 0 = Zero
  fromInteger 1 = Gödel 0 0
  fromInteger 2 = Gödel 1 0
  fromInteger z = (if z2*2<z then (+1) else id)
                  (fromInteger z2 * 2)
   where z2 = z`div`2

  Zero + n = n
  n + Zero = n
  n + m | m>n   = m + n
  Gödel x y + n
   | n<=x       = Gödel (x-n) (y+n)
   | otherwise  = Gödel (x+y+1) 0 + (n-x-1)

  n - Zero = n
  Gödel Zero Zero - Gödel Zero Zero = Zero
  Gödel x y - n
   | n<=y       = Gödel (x+n) (y-n)
   | otherwise  = Gödel 0 (x+y-1) - (n-y-1)

  _ * Zero = Zero
  Zero * _ = Zero
  n * m = n + n*(m-1)

  abs = id
  signum Zero = 0
  signum _ = 1
  negate Zero = Zero

instance Ord Nat where
  compare Zero Zero = EQ
  compare Zero _ = LT
  compare _ Zero = GT
  compare (Gödel x y) (Gödel ξ υ) = compare (x+y, y) (ξ+υ, υ)

instance Enum Nat where
  fromEnum = fromIntegral
  toEnum = fromIntegral

instance Real Nat where
  toRational = fromIntegral

instance Integral Nat where
  toInteger Zero = 0
  toInteger (Gödel x' y') = ((x+y+1)*(x+y))`div`2 + y + 1
   where x = toInteger x'
         y = toInteger y'
  quotRem Zero _ = (Zero, Zero)
  quotRem n d = (fromInteger q, fromInteger r)
   where (q,r) = quotRem (toInteger n) (toInteger d)

treeShow :: Nat -> [String]
treeShow Zero = ["0"]
treeShow (Gödel x y) = zipWith (:) ('┬' : repeat '│') xs
                      ++ zipWith (:) ('└' : repeat ' ') ys
 where xs = treeShow x
       ys = treeShow y

treePrint :: Nat -> IO ()
treePrint = mapM_ putStrLn . treeShow

gnnodes :: Nat -> Int
gnnodes Zero = 0
gnnodes (Gödel x y) = gnnodes x + gnnodes y + 1

gdepth :: Nat -> Int
gdepth Zero = 0
gdepth (Gödel x y) = max (gdepth x) (gdepth y) + 1
