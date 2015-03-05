module Polyhedra

import Vect
import Graph

data Face f = MkFace (Vect n f)

data Polyhedron : Nat -> Nat -> Nat -> Type -> Type where
  MkPolyhedron : Vect k v -> Vect m (Edge v) -> Vect n (Face v) -> Polyhedron k m n v

tetrahedron : Vect 4 v -> Polyhedron 4 6 4 v
tetrahedron [a, b, c, d] = MkPolyhedron verts edges faces where
  verts = [a, b, c, d]
  edges = [(MkEdge a b), (MkEdge b c), (MkEdge c a), (MkEdge a d), (MkEdge d c), (MkEdge d b)]
  faces = [(MkFace [a, b, c]), (MkFace [a, b, d]), (MkFace [a, c, d]), (MkFace [b, c, d])]

cube : Vect 8 v -> Polyhedron 8 12 6 v
cube [a, b, c, d, e, f, g, h] = MkPolyhedron verts edges faces where
  verts = [a, b, c, d, e, f, g, h]
  edges = [(MkEdge a b), (MkEdge b c), (MkEdge c d), (MkEdge d a),
           (MkEdge a e), (MkEdge b f), (MkEdge c g), (MkEdge d h),
           (MkEdge e f), (MkEdge f g), (MkEdge g h), (MkEdge h e)]
  faces = [(MkFace [a, b, c, d]), (MkFace [e, f, g, h]),
           (MkFace [e, a, d, h]), (MkFace [f, b, c, g]),
           (MkFace [e, a, b, f]), (MkFace [h, d, c, g])]
