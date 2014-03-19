module Angluin where
import Data.Char (isDigit, digitToInt)
import Data.List --(find, isInfixOf, isSuffixOf, nub, sort, intersect)
--import System.Random (randomRIO)
--import Test.QuickCheck


data TabVal = Y | N | X

alph = "abcd"

--store states and int, PREFIXES FIRST!!!
a_s :: [(String, Int)]
a_s = [("", 1), ("a", 1), ("c", 0), ("d", 2), ("dc", 3), ("ab", 2)]


a_e = ["", "c"]

--angluin :: IO ()
angluin =
  let
	s = [""]
	e = [""]
	--se = [("", [])]
	wordsInL = []
	getMembership wordsInL w = do
		let y = lookup w wordsInL
		if y == Nothing then do
			x <- wordQuery w '2'
			let wordsInL = (w,x):wordsInL
			return x
		else
			return (((\(Just k) -> k ) y))
	learn wordsInL s e =
		let
			se = [ (a, [ x | b <- e ] ) | a <- s ]
			x = getMembership wordsInL (a++b) 
		--getAutom se
		--se <- [("", [])]
		in return (showAutomaton se)
  in do
	putStrLn "Starting language learning..."
	res <- learn wordsInL s e
	putStrLn ("Exited learning Algorithm. Automaton: \n" ++ res)
			{-getMembership wordsInL w = do
			let y = lookup w wordsInL
			if y == Nothing then do
				x <- wordQuery w '2'
				let wordsInL = (w,x):wordsInL
				return x
			else
				return (((\(Just k) -> k ) y))-}

    {-game xs n = do
      putStr (if n == 1 then "10 " else " " ++ show (11 - n) ++ " ")
      ys <- getGuess [] 4
      res <- return $ answer xs (reverse ys)
      putStrLn (" " ++ concatMap show res)
      if head res == 4 then do
        putStrLn "You won!"
      else game xs (n - 1)

    answer xs ys = [bulls, bears]
      where
        bulls = length $ filter id $ zipWith (==) xs ys
        bears = length (intersect xs ys) - bulls-}

showAutomaton :: [(String, Int)] -> String
showAutomaton a =
	"Initial state: " ++ ((fst . head) a) ++ "\n" ++ (concat [ c ++ (show b) | (c,b) <- a ])


getStates :: [(String, [Int])] -> [String]
getStates = map fst . nubBy (\(x , y) (j, k) -> y == k)

--TODO: use find instead, remove static stuff
getAutom s = [ (s1 , a, lookup (lookup (s1++[a]) s) (map (\(x, y) -> (Just y, x)) s)) | s1 <- getStates s, a <- alph]
	
	
getGuess xs 0 = return xs
getGuess xs n = do
      x <- getChar
      if x `elem` ['0' .. '9'] && not (digitToInt x `elem` xs) then
        getGuess (digitToInt x : xs) (n - 1)
      else do
        -- Deletes the last character from the screen
        putStr "\b \b"
        getGuess xs n


--TODO remove double question when not typing y or n.
wordQuery w 'y' = return 1
wordQuery w 'n' = return 0
wordQuery w a =
	do
		putStrLn $ "Is the word w in the language? Type y or n. w = "++w
		b <- getChar
		wordQuery w b
	
	
{-zu beachten:
leeres Wort (epsilon) wird durch leeren String "" dargestellt.
-}