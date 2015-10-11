module Domain.Domain where

class Domain a where
  join :: a -> a -> a
  meet :: a -> a -> a
  subsumes :: a -> a -> Bool
