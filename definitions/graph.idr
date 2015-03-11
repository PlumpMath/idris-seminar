data Graph : Nat -> Nat -> Type -> Type -> Type where
  MkGraph : Vect m v -> Vect n (Edge k) -> Graph m n v k
