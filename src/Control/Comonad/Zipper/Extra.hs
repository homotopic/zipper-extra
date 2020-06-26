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
, ZipperException(..)
, elemIndexThrow
, ElemNotFoundException(..)
, seekOn
, seekOnThrow
) where

import Control.Applicative
import Control.Monad.Catch
import Control.Comonad.Store
import Control.Comonad.Store.Zipper
import Data.List
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
                               else if null xs then throwM EmptyContentsError
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
zipper' :: (MonadThrow m, Traversable t) => t a -> m (Zipper t a)
zipper' xs = maybe (throwM EmptyZipper) return $ zipper xs

data ElemNotFoundException a = ElemNotFoundException a [a]
    deriving (Show, Eq, Typeable)

instance (Typeable a, Show a) => Exception (ElemNotFoundException a) where
  displayException (ElemNotFoundException x xs) = "Elem " <> show x <> " not found in " <> show xs

-- | Lifted version of `Data.List.elemIndex` that throws an `ElemNotFoundException` if the target does not exist.
elemIndexThrow :: (MonadThrow m, Eq a, Typeable a, Show a) => a -> [a] -> m Int
elemIndexThrow x xs = case elemIndex x xs of
  Nothing -> throwM $ ElemNotFoundException x xs
  Just a -> return a

-- | Seek on a property of the elements of the zipper. Finds the index of the element to search for
--   and moves the tape to that position.
seekOn :: Eq b => (a -> b) -> b -> Zipper [] a -> Maybe (Zipper [] a)
seekOn f x ys = do
  k <- elemIndex x (f <$> unzipper ys)
  return $ seek k ys

-- | Lifted version of `seekOn` which throws an `ElemNotFoundException` if the target does not exist.
seekOnThrow :: (MonadThrow m, Eq b, Typeable b, Show b) => (a -> b) -> b -> Zipper [] a -> m (Zipper [] a)
seekOnThrow f x ys = do
  k <- elemIndexThrow x (f <$> unzipper ys)
  return $ seek k ys
