---- Part 1: Basic structural recursion ----------------

-- 1. Merge sort

-- Deal a list into two (almost) equal-sized lists by alternating elements
-- For example, deal [1,2,3,4,5,6,7] = ([1,3,5,7], [2,4,6])
-- and          deal   [2,3,4,5,6,7] = ([2,4,6], [3,5,7])
-- Hint: notice what's happening between the answers to deal [2..7] and
-- deal (1:[2..7]) above to get an idea of how to approach the recursion
deal :: [a] -> ([a], [a])
deal [] = ([], [])
deal (x : xs) =
  let (ys, zs) = deal xs
   in (x : zs, ys)

-- Now implement merge and mergesort (ms), and test with some
-- scrambled lists to gain confidence that your code is correct
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  -- we keep the smallest value then call merge on the
  -- rest of the list until we end up with something like
  -- 1:2:3:4:5[] from [1,2,3] [4,5]
  | x <= y = x : merge xs (y : ys)
  | x > y = y : merge (x : xs) ys

ms :: (Ord a) => [a] -> [a]
ms [] = []
ms [x] = [x]
-- general case: deal, recursive call, merge
ms xs =
  let (ys, zs) = deal xs -- here we capture the two output arrays
   in merge (ms ys) (ms zs) -- call the ms function on the newly split arrays then merge the final values

-- 2. A backward list data structure

-- Back Lists: Lists where elements are added to the back ("snoc" == rev "cons")
-- For example, the list [1,2,3] is represented as Snoc (Snoc (Snoc Nil 1) 2) 3
data BList a = Nil | Snoc (BList a) a deriving (Show, Eq)

-- Add an element to the beginning of a BList, like (:) does
cons :: a -> BList a -> BList a
cons n Nil = Snoc Nil n -- inner list means add this snoc
cons n (Snoc bl n2) = Snoc (cons n bl) n2 -- call until you reach inner list

-- Convert a usual list into a BList (hint: use cons in the recursive case)
toBList :: [a] -> BList a
toBList [] = Nil
toBList (x : xs) = cons x $ toBList xs

-- Add an element to the end of an ordinary list, like Snoc does
snoc :: [a] -> a -> [a]
-- once we reach the end of the node, send back the list with the element
snoc [] n = [n] 
-- whenever we're not at the end, send back xs with the element to add
-- until we reach the base case
snoc (x:xs) n = x:(snoc xs n)



-- Convert a BList into an ordinary list (hint: use snoc in the recursive case)
fromBList :: BList a -> [a]
-- whenever we reach nil and n then just return [n]
fromBList (Snoc Nil n) = [n]
-- whenever we're not at the innermost layer, keep moving inward by sending bl
-- then once the recursion ends, use snoc to build the array by adding to the back
fromBList (Snoc bl x) = snoc (fromBList bl) x 

-- 3. A binary tree data structure
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- Count number of Empty's in the tree
num_empties :: Tree a -> Int
-- if we find an empty send a 1 back
num_empties Empty = 1
-- whenever we're not at empty nodes, send the next two children of the cur node
-- then recursively add them back up to count the number of empties
num_empties ( Node _ (left) (right)) = num_empties left + num_empties right 



-- Count number of Node's in the tree
num_nodes :: Tree a -> Int
-- if we hit empty then just don't accumulate
num_nodes Empty = 0
-- if we have a node then increment by 1 and recursively call num_nodes on the left and right
num_nodes (Node _ left right) = 1 + (num_nodes left) + (num_nodes right)
 

-- Insert a new node in the leftmost spot in the tree
insert_left :: a -> Tree a -> Tree a
-- if we hit the empty on the left side then insert the new node
insert_left val Empty = Node val Empty Empty
-- if we don't hit empty immediately, then traverse down the left
insert_left val (Node n left right) = Node n (insert_left val left) right 

-- Insert a new node in the rightmost spot in the tree
insert_right :: a -> Tree a -> Tree a
-- if we hit the empty on the right side then insert the new node
insert_right val Empty = Node val Empty Empty
-- if we don't hit empty immediately, then traverse down the right
insert_right val (Node n left right) = Node n left (insert_right val right)

-- Add up all the node values in a tree of numbers
sum_nodes :: (Num a) => Tree a -> a
-- if we hit empty then add 0 and end the recursion
sum_nodes Empty = 0
-- if we don't hit empty then add the value inside the node then
-- call sum_nodes on the left and right to add the rest of the values
sum_nodes (Node val left right) = val + (sum_nodes left) + (sum_nodes right)

-- Produce a list of the node values in the tree via an inorder traversal
-- Feel free to use concatenation (++)
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node val left right) = (inorder left) ++ [val] ++ (inorder right)

---- Part 2: Iteration and Accumulators ----------------

-- Both toBList and fromBList from Part 1 Problem 2 are O(n^2) operations.
-- Reimplement them using iterative helper functions (locally defined using
-- a 'where' clause) with accumulators to make them O(n)
toBList' :: [a] -> BList a
toBList' = undefined

fromBList' :: BList a -> [a]
fromBList' = undefined

-- Even tree functions that do multiple recursive calls can be rewritten
-- iteratively using lists of trees and an accumulator. For example,
sum_nodes' :: (Num a) => Tree a -> a
sum_nodes' t = sum_nodes_it [t] 0
  where
    sum_nodes_it :: (Num a) => [Tree a] -> a -> a
    sum_nodes_it [] a = a
    sum_nodes_it (Empty : ts) a = sum_nodes_it ts a
    sum_nodes_it (Node n t1 t2 : ts) a = sum_nodes_it (t1 : t2 : ts) (n + a)

-- Use the same technique to convert num_empties, num_nodes, and sum_nodes2
-- into iterative functions with accumulators

num_empties' :: Tree a -> Int
num_empties' = undefined

num_nodes' :: Tree a -> Int
num_nodes' = undefined

sum_nodes2' :: (Num a) => Tree a -> a
sum_nodes2' = undefined

---- Part 3: Higher-order functions ----------------

-- The functions map, all, any, filter, dropWhile, takeWhile, and break
-- from the Prelude are all higher-order functions. Reimplement them here
-- as list recursions. break should process each element of the list at
-- most once. All functions should produce the same output as the originals.

my_map :: (a -> b) -> [a] -> [b]
my_map = undefined

my_all :: (a -> Bool) -> [a] -> Bool
my_all = undefined

my_any :: (a -> Bool) -> [a] -> Bool
my_any = undefined

my_filter :: (a -> Bool) -> [a] -> [a]
my_filter = undefined

my_dropWhile :: (a -> Bool) -> [a] -> [a]
my_dropWhile = undefined

my_takeWhile :: (a -> Bool) -> [a] -> [a]
my_takeWhile = undefined

my_break :: (a -> Bool) -> [a] -> ([a], [a])
my_break = undefined

-- Implement the Prelude functions and, or, concat using foldr

my_and :: [Bool] -> Bool
my_and = undefined

my_or :: [Bool] -> Bool
my_or = undefined

my_concat :: [[a]] -> [a]
my_concat = undefined

-- Implement the Prelude functions sum, product, reverse using foldl

my_sum :: (Num a) => [a] -> a
my_sum = undefined

my_product :: (Num a) => [a] -> a
my_product = undefined

my_reverse :: [a] -> [a]
my_reverse = undefined
