module Matrices

import Data.Vect

%default total

eigenvalues : Vect 2 (Vect 2 Float) -> (Float, Float)
eigenvalues [[a, b], [c, d]] = (λ1, λ2) where
  λ1 = (0.5*((a+d)+(prim__floatSqrt (((a+d)*(a+d))-(4.0*(a*d - b*c))))))
  λ2 = (0.5*((a+d)-(prim__floatSqrt (((a+d)*(a+d))-(4.0*(a*d - b*c))))))

determinant : Vect 2 (Vect 2 Float) -> Float
determinant [[a, b], [c, d]] = (a * d) - (b * c)
