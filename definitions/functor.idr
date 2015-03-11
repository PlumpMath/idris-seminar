class Functor (f : Type -> Type) where
  map : (m : a -> b) -> f a -> f b
