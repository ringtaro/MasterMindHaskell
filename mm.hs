import System.Random

answerLength :: Int
answerLength = 4

type MindChars = String

createAnswer :: StdGen -> MindChars
createAnswer = take answerLength . randomRs ('0','9')

getHitsBlows :: MindChars -> MindChars -> (Int, Int)
getHitsBlows answer line = 
  let
    hits = length $ filter (uncurry (==)) $ zip answer line
    blows = length $ filter (\a -> any (== a) line) answer
  in (hits, blows - hits)

main :: IO ()
main = do
  gen <- getStdGen
  let
    answer = createAnswer gen
  putStrLn $ "Game start! Please input " ++ (show answerLength) ++ " numbers."
  mainloop answer 1

mainloop :: MindChars -> Int -> IO ()
mainloop answer times = do
  line <- getMindChars
  let
    (hits, blows) = getHitsBlows answer line
  if hits == answerLength
  then do
    putStrLn $ "All hits! " ++ (show times) ++ " Times" 
    return ()
  else do
    putStrLn $ "Hits: " ++ (show hits) ++ " Blows: " ++ (show blows)
    mainloop answer $ times + 1

getMindChars :: IO MindChars
getMindChars = do
  line <- getLine
  if isMindChars line
  then
    return line
  else do
    putStrLn $ "Please input " ++ (show answerLength) ++ " numbers."
    getMindChars

isMindChars :: String -> Bool
isMindChars line = length line == answerLength && all (\x -> elem x ['0'..'9']) line
