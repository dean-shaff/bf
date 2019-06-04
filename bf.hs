data State a = State [a] Int deriving (Show)

right :: (Num a) => State a -> State a
right (State xs idx) = (State xs (idx + 1))

left :: (Num a) => State a -> State a
left (State xs idx) = State xs (idx - 1)

incr :: (Num a) => State a -> State a
incr (State xs idx) =
  State (take idx xs  ++ [(xs!!idx) + 1] ++ drop (idx+1) xs) idx

decr :: (Num a) => State a -> State a
decr (State xs idx) =
  State (take idx xs  ++ [(xs!!idx) - 1] ++ drop (idx+1) xs) idx

getEnclosedCmds :: String -> String
getEnclosedCmds cmds = cmds

interpret :: (Num a) => String -> State a -> State a
interpret cmd:xs state
  | cmd == '>' = interpret xs (right state)
  | cmd == '<' = interpret xs (left state)
  | cmd == '+' = interpret xs (incr state)
  | cmd == '-' = interpret xs (decr state)
  | cmd == ']' = interpret xs state
  | cmd == '[' =
    let chunk = getEnclosedCmds cmd:xs
        interpretChunk = interpret chunk
        loop s
          | (getVal s) == 0 = s
          | otherwise = loop (interpretChunk s)
        state' = interpret (take (length chunk) cmds) (loop state)
    in state'
interpret [] state = state


main = do
  let program = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."
  let state = State (map (\x -> 0) [1..100]) 0
  print
  -- print (getVal list)
  -- print (getVal $ right $ left $ right list)
