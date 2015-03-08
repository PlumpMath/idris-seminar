module Bivect

%default total

data Bivect : Nat -> Type -> Type -> Type where
  Nil : Bivect Z a b
  (::) : Either a b -> Bivect n a b -> Bivect (S n) a b

class Bifoldable (p : Type -> Type -> Type) where
  bifoldr : (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c
  bifoldl : (c -> a -> c) -> (c -> b -> c) -> c -> p a b -> c

instance Bifoldable (Bivect n) where
  bifoldr f g z Nil = z
  bifoldr f g z (Left x :: xs) = f x (bifoldr f g z xs)
  bifoldr f g z (Right x :: xs) = g x (bifoldr f g z xs)
  bifoldl f g z Nil = z
  bifoldl f g z (Left x :: xs) = bifoldl f g (f z x) xs
  bifoldl f g z (Right x :: xs) = bifoldl f g (g z x) xs

