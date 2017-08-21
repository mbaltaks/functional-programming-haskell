-- https://www.futurelearn.com/courses/functional-programming-haskell/1/steps/116718

data Tree = Leaf | Node Int Tree Tree deriving Show


-- provided
treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) =
  1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)


-- exercise, add all the Node values
treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node value left right) =
    value + (treeSum left) + (treeSum right)


-- provided
isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
    let leftSorted   = isSortedTree leftSubtree minVal x
        rightSorted = isSortedTree rightSubtree x maxVal
    in x >= minVal && x< maxVal && leftSorted && rightSorted

-- call with isSortedTree (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) minBound maxBound


-- provided
addNewMax :: Tree -> Tree
-- add a new max element to tree
addNewMax Leaf = Node 0 Leaf Leaf  -- input tree with no nodes
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)  -- this is the rightmost Node
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2) -- intermediate node, go down right subtree
