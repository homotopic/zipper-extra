module Control.Comonad.Zipper.Extra (
  Control.Comonad.Store.Zipper.Zipper
, Control.Comonad.Store.Zipper.zipper
, Control.Comonad.Store.Zipper.zipper1
, Control.Comonad.Store.Zipper.unzipper
, Control.Comonad.Store.Zipper.size
, paginate
, zipperNextMaybe
, zipperPreviousMaybe
, zipperWithin
) where

import Control.Comonad.Store
import Control.Comonad.Store.Zipper
import Data.List.Split

-- | Turn a list into a zipper of chunks of length n
paginate :: Int -> [a] -> Maybe (Zipper [] [a])
paginate n = zipper . chunksOf n

-- | Return the peek of the next element if it exists.
zipperNextMaybe :: Zipper t a -> Maybe a
zipperNextMaybe xs = if pos xs < size xs-1 then Just (peeks (+1) xs) else Nothing

-- | Return the peek of the previous element if it exists.
zipperPreviousMaybe :: Zipper t a -> Maybe a
zipperPreviousMaybe xs = if pos xs > 0 then Just (peeks (+ (-1)) xs) else Nothing

-- Return a list of elements within 'r' hops either side of the zipper target.
zipperWithin :: Int -> Zipper t a -> [a]
zipperWithin r xs = (`peek` xs) <$>  [(max 0 (pos xs - r)) .. (min (size xs -1) (pos xs + r))]

