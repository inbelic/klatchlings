module Internal.Boolean where

class Boolean a where
  both :: a -> a -> a
  oneOf :: a -> a -> a

instance Boolean Bool where
  both = (&&)
  oneOf = (||)
