data LinkedList a = Empty | Node a (LinkedList a) (LinkedList a) deriving (Show)

getVal :: (Num a) => LinkedList a -> a
getVal (Node val left right) = val

incr :: (Num a) => LinkedList a -> LinkedList a
incr Empty = Empty
incr (Node val left' right') = Node (val + 1) left' right'

decr :: (Num a) => LinkedList a -> LinkedList a
decr Empty = Empty
decr (Node val left' right') = Node (val - 1) left' right'

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

main do
  let list = foldl (\accum x -> insert x (left accum) accum) (Node 0 Empty Empty) [1..3]
  print (getVal $ left $ right $ incr $ incr list)
