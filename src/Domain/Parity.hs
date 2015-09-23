module Domain.Parity where

import Domain.Domain

data Parity = Bot | Odd | Even | Top
    deriving (Show, Eq, Ord)

instance Domain Parity where
  join Bot a = a
  join a Bot = a
  join a b = if a == b then a else Top
  meet Top a = a
  meet a Top = a
  meet a b = if a == b then a else Bot