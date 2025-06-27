module Queue1 (Queue, mtq, addq, remq) where

---- Interface ----------------
mtq  :: Queue a                        -- empty queue
addq :: a -> Queue a -> Queue a        -- add element to back of queue
remq :: Queue a -> Maybe (a, Queue a)  -- remove element from front of queue

---- Implementation -----------

{- In this implementation, a queue is represented as an ordinary list.
The "front" of the queue is the head of the list, and the "back" of
the queue is the end of the list. Elements are removed from the front,
and new elements are added to the back.
-}

data Queue a = Queue1 [a] -- add "deriving Show" for local testing

mtq = undefined
addq x q = undefined
remq q = undefined
