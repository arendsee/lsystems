module Lsystem.Generator
(
    next
  , generate
  , Nested(..)
) where

class Nested f where
  unnest :: f (f a) -> f a
  nest :: a -> f a

instance Nested [] where 
  unnest = concat
  nest x = [x] 

next :: (Nested f, Functor f, Eq a) => [(a, f a)] -> f a -> f a
next rs xs = unnest $ fmap (apply' rs) xs where
  apply' rs' x = case (lookup x rs') of
    Nothing -> nest x
    Just x' -> x'

generateForever :: (Nested f, Functor f, Eq a) => [(a, f a)] -> f a -> [f a]
generateForever rs xs = [next'] ++ generateForever rs next'  where
  next' = next rs xs

generate :: (Nested f, Functor f, Eq a) => Int -> [(a, f a)] -> f a -> [f a]
generate i rs xs = take i $ generateForever rs xs 
