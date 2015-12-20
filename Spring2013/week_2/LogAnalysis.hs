--
-- The following code is messy, needs to be improved.
--

{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log


parseMessage :: String -> LogMessage
parseMessage s
  | h == 'E' = LogMessage (Error $ read (sl !! 1)) (read (sl !! 2)) (unwords $ drop 3 sl)
  | h == 'I' = LogMessage Info (read (sl !! 1)) (unwords $ drop 2 sl)
  | h == 'W' = LogMessage Warning (read (sl !! 1)) (unwords $ drop 2 sl)
  | otherwise = Unknown s
  where h = head s
        sl = words s


parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert l Leaf = Node Leaf l Leaf
insert l@(LogMessage _ lts _) (Node ln n@(LogMessage _ nts _) rn)
  | lts <= nts = Node (insert l ln) n rn
  | otherwise = Node ln n (insert l rn)


build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt n rt) = inOrder lt ++ n:(inOrder rt)


filterSeverity :: LogMessage -> Bool
filterSeverity (LogMessage (Error s) _ _)
  | s >=50 = True
  | otherwise = False
filterSeverity _ = False


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ls = map (\(LogMessage _ _ l) -> l) . inOrder . build $ filter filterSeverity ls
