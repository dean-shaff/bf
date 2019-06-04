data LinkedList a = Empty | Node a (LinkedList a) (LinkedList a) deriving (Show)

getVal :: (Num a) => LinkedList a -> a
getVal (Node val left right) = val

right :: (Num a) => LinkedList a -> LinkedList a
right Empty = Empty
right (Node val left' right') = right'

left :: (Num a) => LinkedList a -> LinkedList a
left Empty = Empty
left (Node val left' right') = left'

insert :: (Num a) => a -> LinkedList a -> LinkedList a -> LinkedList a
insert val left' Empty = Node val left' Empty
insert val left' right' =
  let x = getVal right'
      rightLeft = left right'
      rightRight = right right'
  in Node x left' (insert val right' rightRight)


-- data State a = State [a] Int deriving (Show)
--
-- right :: (Num a) => State a -> State a
-- right (State xs idx) = (State xs (idx + 1))
--
-- left :: (Num a) => State a -> State a
-- left (State xs idx) = State xs (idx - 1)
--
-- incr :: (Num a) => State a -> State a
-- incr (State xs idx) =
--   State (take idx xs  ++ [(xs!!idx) + 1] ++ drop (idx+1) xs) idx
--
-- decr :: (Num a) => State a -> State a
-- decr (State xs idx) =
--   State (take idx xs  ++ [(xs!!idx) - 1] ++ drop (idx+1) xs) idx


main = do
  let program = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
  let list = foldl (\accum x -> insert x (left accum) accum) (Node 0 Empty Empty) [1..3]
  print (getVal list)
  print (getVal $ right $ left $ right list)
