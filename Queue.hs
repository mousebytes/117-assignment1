-- Queue client

-- Choose one of the following two imports only
import Queue1
--import Queue2


-- A Queue client

-- A data type representing queue operations
-- Queue operation (A = add, R = remove)
data Qop a = A a | R

-- Perform a list of queue operations on an emtpy queue,
-- returning the list of the removed elements (in the order removed)
-- Hint: use a helper function with the queue as accumulator
perf :: [Qop a] -> [a]
perf ops = undefined

-- Now test the above function thouroughly for different types a.
-- For example, here is one test with a = Int:
--    perf [A 3, A 5, R, A 7, R, A 9, A 11, R, R, R] ---> [3,5,7,9,11]


