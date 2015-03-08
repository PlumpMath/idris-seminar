module Trivect

%default total

data Trivect : Nat -> Type -> Type -> Type -> Type where
  Nil : Trivect Z a b c
  (::) : Either (Either a b) c -> Trivect n a b c -> Trivect (S n) a b c

class Trifoldable (p : Type -> Type -> Type -> Type) where
  trifoldr : (a -> d -> d) -> (b -> d -> d) -> (c -> d -> d) -> d -> p a b c -> d
  trifoldl : (d -> a -> d) -> (d -> b -> d) -> (d -> c -> d) -> d -> p a b c -> d

instance Trifoldable (Trivect n) where
  trifoldr f g h z Nil = z
  trifoldr f g h z (Left (Left x) :: xs) = f x (trifoldr f g h z xs)
  trifoldr f g h z (Left (Right x) :: xs) = g x (trifoldr f g h z xs)
  trifoldr f g h z (Right x :: xs) = h x (trifoldr f g h z xs)
  trifoldl f g h z Nil = z
  trifoldl f g h z (Left (Left x) :: xs) = trifoldl f g h (f z x) xs
  trifoldl f g h z (Left (Right x) :: xs) = trifoldl f g h (g z x) xs
  trifoldl f g h z (Right x :: xs) = trifoldl f g h (h z x) xs


