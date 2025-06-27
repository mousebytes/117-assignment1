module Queue2 (Queue, mtq, addq, remq) where

---- Interface ----------------
mtq  :: Queue a                        -- empty queue
addq :: a -> Queue a -> Queue a        -- add element to back of queue
remq :: Queue a -> Maybe (a, Queue a)  -- remove element from front of queue

--- Implementation -----------

{- In this implementation, a queue is represented as a pair of lists,
a "front part" and a reversed "back part". Elements are removed from
the head of the front part, and new elements are added to the front of
the back part. If the front part becomes empty, the back part is reversed
and becomes the new front part, leaving the back part empty.
Invariant: if the front is empty, the back is also empty.
-}

data Queue a = Queue2 [a] [a]  -- add "deriving Show" for local testing

mtq = undefined
addq x q = undefined
remq q = undefined
