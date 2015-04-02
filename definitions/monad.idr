class Applicative m => Monad (m : Type -> Type) where
  (>>=) : m a -> (a -> m b) -> m b
