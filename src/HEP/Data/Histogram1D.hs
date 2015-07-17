module HEP.Data.Histogram1D
       (
         Hist1D (..)
       , histogram
       , scaleHist
       , integrate
       , add
       , sub
       ) where

-- import Pipes

newtype Hist1D a = Hist1D { getHist :: Maybe [(a, Double)] } deriving Show

instance Eq a => Monoid (Hist1D a) where
  mempty = Hist1D Nothing
  mappend = add

add :: Eq a => Hist1D a -> Hist1D a -> Hist1D a
add = combine (+)

sub :: Eq a => Hist1D a -> Hist1D a -> Hist1D a
sub = combine (-)

combine :: Eq a => (Double -> Double -> Double) -> Hist1D a -> Hist1D a -> Hist1D a
combine _ (Hist1D Nothing)   hist               = hist
combine _ hist               (Hist1D Nothing)   = hist
combine f (Hist1D (Just h1)) (Hist1D (Just h2)) =
  let (bin1, x1) = unzip h1
      (bin2, x2) = unzip h2
  in Hist1D $ if bin1 /= bin2
              then Nothing
              else Just (zip bin1 (zipWith f x1 x2))

histogram :: (Fractional a, Ord a) =>
             Int  -- ^ Number of bins
          -> a    -- ^ Lower bound
          -> a    -- ^ Upper bound
          -> [a]  -- ^ Data
          -> Hist1D a
histogram nbin lo hi xs
  | hi <= lo  = Hist1D Nothing
  | otherwise = let bins = binList nbin lo hi
                    lowerUpper = zip bins (tail bins)
                    hist = map (flip (uncurry count) xs) lowerUpper
                in Hist1D $ Just (zip bins hist)

binList :: (Fractional a, Num a) => Int -> a -> a -> [a]
binList nbin lo hi = take (nbin + 1) $ iterate (+ binsize) lo
  where binsize = (hi - lo) / fromIntegral nbin

count :: Ord a => a -> a -> [a] -> Double
count lo hi = fromIntegral . length . filter ((&&) <$> (>= lo) <*> (< hi))

scaleHist :: Double -> Hist1D a -> Hist1D a
scaleHist _ (Hist1D Nothing)  = Hist1D Nothing
scaleHist s (Hist1D (Just h)) = Hist1D $ (Just . map (\(b, x) -> (b, s*x))) h

integrate :: Hist1D a -> Double
integrate (Hist1D Nothing)  = 0
integrate (Hist1D (Just h)) = foldr (\(_, x) i -> x + i) 0 h
