module Graph

data Edge k = MkEdge k k
data Graph k v = MkGraph (List v) (List (Edge k))
