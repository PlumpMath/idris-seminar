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

zip : Vect n a -> Vect n b -> Vect n (a, b)
zip Nil Nil = Nil
zip (x :: xs) (y :: ys) = (x, y) :: (zip xs ys)

rotations : Vect n a -> Vect n (Vect n a)
rotations {n} v = rotates n v where
  rotates : (m : Nat) -> Vect n a -> Vect m (Vect n a)
  rotates Z v = Nil
  rotates (S k) v = v :: (rotates k (rotate v)) where
    rotate : {n : Nat} -> Vect n a -> Vect n a
    rotate Nil = Nil
    rotate (x :: xs) = push x xs where
      push : {n : Nat} -> a -> Vect n a -> Vect (S n) a
      push x Nil = x :: Nil
      push x (y :: ys) = y :: push x ys

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
  
FoldlN : Vect (S n) Type -> Type -> Type
FoldlN v b = N (M v b) -> b -> N v -> b where
  M : Vect n Type -> Type -> Vect n Type
  M v b = map f v where
    f : Type -> Type
    f a = (a -> b -> b)
  N : Vect n Type -> Type
  N Nil = Type
  N (x :: xs) = Type -> N xs

class FoldableN (v : Vect (S n) Type) where
  foldlN : FoldlN v b


