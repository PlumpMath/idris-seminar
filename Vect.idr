module Vect

%default total

data Vect : Nat -> Type -> Type where
  Nil : Vect 0 a
  (::) : a -> Vect n a -> Vect (S n) a

head : Vect (S n) a -> a
head (x :: xs) = x

tail : Vect (S n) a -> Vect n a
tail (x :: xs) = xs

repeat : (n : Nat) -> a -> Vect n a
repeat Z x = Nil
repeat (S k) x = x :: repeat k x

instance Functor (Vect n) where
  map f Nil = Nil
  map f (x :: xs) = (f x) :: (map f xs)

instance Applicative (Vect n) where
  pure {n} = repeat n
  Nil <$> Nil = Nil
  (f :: fs) <$> (x :: xs) = (f x) :: (fs <$> xs)

instance Monad (Vect n) where
  (>>=) Nil f = Nil
  (>>=) xs f = diag (map f xs) where
    diag : Vect n (Vect n b) -> Vect n b
    diag Nil = Nil
    diag ((y :: ys) :: yss) = y :: diag (map tail yss)

instance Foldable (Vect n) where
  foldr f a Nil = a
  foldr f a (x :: xs) = f x (foldr f a xs)
  foldl f a Nil = a
  foldl f a (x :: xs) = foldl f (f a x) xs

