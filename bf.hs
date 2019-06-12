import Debug.Trace

data State a = State [a] Int deriving (Show)

val :: (Num a, Eq a) => State a -> a
val (State xs idx) = xs!!idx

right :: (Num a, Eq a) => State a -> State a
right (State xs idx) = State xs (idx + 1)

left :: (Num a, Eq a) => State a -> State a
left (State xs idx) = State xs (idx - 1)

incr :: (Num a, Eq a) => State a -> State a
incr (State xs idx) =
  State (take idx xs  ++ [(xs!!idx) + 1] ++ drop (idx + 1) xs) idx

decr :: (Num a, Eq a) => State a -> State a
decr (State xs idx) =
  State (take idx xs  ++ [(xs!!idx) - 1] ++ drop (idx + 1) xs) idx

getEnclosedCmds :: String -> String
getEnclosedCmds cmds =
  let getMatchingBracket cmds count end
        | length cmds == 0 = retTuple
        | count == 0 = retTuple
        | otherwise =
          let cmd = head cmds
              count' = case cmd of
                         '[' -> count + 1
                         ']' -> count - 1
                         _ -> count
              end' = end + 1
          in getMatchingBracket (tail cmds) count' end'
        where retTuple = (count, end)
      end = snd $ getMatchingBracket cmds 1 0
  in take end cmds


loop :: (Num a, Eq a) => (State a -> IO (State a)) -> IO (State a) -> IO (State a)
loop untilFn state =
  state >>= (\s ->
    if val s == 0
    then return s
    else loop untilFn (untilFn s))

interpret :: String -> State Int -> IO (State Int)
interpret [] state = return state
interpret cmds state
  | x == '>' = interpret xs (right state)
  | x == '<' = interpret xs (left state)
  | x == '+' = interpret xs (incr state)
  | x == '-' = interpret xs (decr state)
  | x == '.' = putChar (toEnum (val state)::Char) >> interpret xs state
  | x == '[' =
    let chunk = getEnclosedCmds xs
        interpretChunk = interpret chunk
        dropped = drop (length chunk + 1) cmds
        newState = (loop interpretChunk (return state))
        res' = newState >>= (interpret dropped)
    in res'
  | otherwise = interpret xs state
  where x = head cmds
        xs = tail cmds

main :: IO ()
main = do
  let program = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
  let state = State (map (\x -> 0) [1..100]) 0
  interpret program state
  return ()
