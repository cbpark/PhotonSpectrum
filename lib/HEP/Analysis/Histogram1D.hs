module HEP.Analysis.Histogram1D
       (
         Hist1D (..)
       , histogram
       , scaleHist
       , integrate
       , add
       , sub
       ) where

import           Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as V

newtype Hist1D a = Hist1D { getHist :: Maybe (Vector (a, Double)) }
                 deriving Show

instance (Eq a, Unbox a) => Monoid (Hist1D a) where
  mempty = Hist1D Nothing
  mappend = add

add :: (Eq a, Unbox a) => Hist1D a -> Hist1D a -> Hist1D a
add = combine (+)

sub :: (Eq a, Unbox a) => Hist1D a -> Hist1D a -> Hist1D a
sub = combine (-)

combine :: (Eq a, Unbox a) =>
           (Double -> Double -> Double) -> Hist1D a -> Hist1D a -> Hist1D a
combine _ (Hist1D Nothing)   hist               = hist
combine _ hist               (Hist1D Nothing)   = hist
combine f (Hist1D (Just h1)) (Hist1D (Just h2)) =
  let (bin1, x1) = V.unzip h1
      (bin2, x2) = V.unzip h2
  in Hist1D $ if bin1 /= bin2
              then Nothing
              else Just (V.zip bin1 (V.zipWith f x1 x2))

histogram :: (Fractional a, Ord a, Unbox a) =>
             Int  -- ^ Number of bins
          -> a    -- ^ Lower bound
          -> a    -- ^ Upper bound
          -> [a]  -- ^ Data
          -> Hist1D a
histogram nbin lo hi xs
  | hi <= lo  = Hist1D Nothing
  | otherwise = let bins = binList nbin lo hi
                    lowhigh = V.zip bins (V.tail bins)
                    hist = V.map (flip (uncurry count) (V.fromList xs)) lowhigh
                in Hist1D $ Just (V.zip bins hist)

binList :: (Fractional a, Num a, Unbox a) => Int -> a -> a -> Vector a
binList nbin lo hi = V.iterateN (nbin + 1) (+ binsize) lo
  where binsize = (hi - lo) / fromIntegral nbin

count :: (Ord a, Unbox a) => a -> a -> Vector a -> Double
count lo hi = fromIntegral . V.length . V.filter ((&&) <$> (>= lo) <*> (< hi))

scaleHist :: Unbox a => Double -> Hist1D a -> Hist1D a
scaleHist _ (Hist1D Nothing)  = Hist1D Nothing
scaleHist s (Hist1D (Just h)) = Hist1D $ (Just . V.map (\(b, x) -> (b, s*x))) h

integrate :: Unbox a => Hist1D a -> Double
integrate (Hist1D Nothing)  = 0
integrate (Hist1D (Just h)) = V.foldr (\(_, x) i -> x + i) 0 h
