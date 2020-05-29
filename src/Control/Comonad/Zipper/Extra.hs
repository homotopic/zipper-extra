module Control.Comonad.Zipper.Extra (
  Control.Comonad.Store.Zipper.Zipper
, Control.Comonad.Store.Zipper.zipper
, Control.Comonad.Store.Zipper.zipper1
, Control.Comonad.Store.Zipper.unzipper
, Control.Comonad.Store.Zipper.size
, paginate
, paginate'
, PaginationException(..)
, zipperNextMaybe
, zipperPreviousMaybe
, zipperWithin
, zipper'
) where

import Control.Monad.Catch
import Control.Exception
import Control.Comonad.Store
import Control.Comonad.Store.Zipper
import Data.List.Split
import Data.Typeable

-- | Turn a list into a zipper of chunks of length n
paginate :: Int -> [a] -> Maybe (Zipper [] [a])
paginate n = zipper . chunksOf n

data PaginationException = EmptyContentsError | ZeroPageSize | UnknownPaginationException
  deriving (Show, Eq, Typeable)

instance Exception PaginationException where
  displayException EmptyContentsError         = "Can not create a Zipper of length zero."
  displayException ZeroPageSize               = "Can not divide into pages of size zero."
  displayException UnknownPaginationException = "Unknown pagination exception."

-- | Like `paginate`, but throw an exception if it can't create the zipper.
paginate' :: MonadThrow m => Int -> [a] -> m (Zipper [] [a])
paginate' n xs = case paginate n xs of
                    Just x -> return x
                    Nothing -> if n == 0 then throwM ZeroPageSize
                               else if length xs == 0 then throwM EmptyContentsError
                               else throwM UnknownPaginationException

-- | Return the peek of the next element if it exists.
zipperNextMaybe :: Zipper t a -> Maybe a
zipperNextMaybe xs = if pos xs < size xs-1 then Just (peeks (+1) xs) else Nothing

-- | Return the peek of the previous element if it exists.
zipperPreviousMaybe :: Zipper t a -> Maybe a
zipperPreviousMaybe xs = if pos xs > 0 then Just (peeks (+ (-1)) xs) else Nothing

-- | Return a list of elements within 'r' hops either side of the zipper target.
zipperWithin :: Int -> Zipper t a -> [a]
zipperWithin r xs = (`peek` xs) <$>  [(max 0 (pos xs - r)) .. (min (size xs -1) (pos xs + r))]

data ZipperException = EmptyZipper
  deriving (Show, Eq, Typeable)

instance Exception ZipperException where
  displayException EmptyZipper = "Can not create an empty zipper."

-- | Like `zipper` but lifted to `MonadThrow`.
zipper' xs = maybe (throwM EmptyZipper) return $ zipper xs
