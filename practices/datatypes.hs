data List a = Empty | Node a (List a)
 deriving (Show)

ins x Empty = Node x Empty
ins x ls = Node x ls

max' (Node k Empty) = k
max' (Node k ls) = max k (max' ls)
