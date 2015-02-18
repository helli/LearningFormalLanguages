module Angluin where

import Data.List
--import Data.Function

--TODO: hide some of these to make for a better :browse

-- WordLists are sorted set of words.
type WordList = [String]

-- stores all the words and their membership in L, get membership via "lookup w wl" for some word  w and lookup table wl
type WordsAndTheirMembership = [(String, Bool)]

-- an alphabet is a (sorted) set of chars
--type Alph = [Char]
type Alph = String

{-
The "state" of the learning algorithm is stored in this Object. It consists of:
-the identifier Tab
-the List of known prefixes (S)
-the List of known extensions (E)
-the lookup table of words whose membership is known
-the alphabet over which the algorithm is executed
-}
data Table = Tab WordList WordList WordsAndTheirMembership Alph --TODO: use newtype or type ?

--some sample "state" of the algorithm
tabA :: Table
tabA = Tab s e words sigma
	where
		s = [""]
		e = [""]
		words = [("", False), ("a", True), ("b", True)]
		sigma = "ab"

--complete but not consistent
tabB = Tab s e w sigma
	where
		s = ["","c","cc"]
		e = [""]
		w = [("",False),("c",False),("cc",True), ("a",False),("b",False),("ca",False),("cb",False),("cca",True),("ccb",False),("ccc",False)]
		sigma = "abc"

		
--complete and consistent
tabC = Tab s e w sigma
	where
		s = ["", "a", "c", "ab"]
		e = ["", "b"]
		--is there a better lookup if this is sorted?
		--Maybe in Data.List.Ordered
		--or in Data.Sorted
		w = [("", False), ("a", False), ("c", True), ("ab", True), ("b", False), ("ab", True), ("cb", True), ("abb", True),("bb",False),("d",True),("db",True),("aa",False),("aab",True),("ac",True),("acb",True),("ad",True),("adb",True),("ca",True),("cab",True),("cbb",True),("cc",True),("ccb",True),("cd",True),("cdb",True),("aba",True),("abab",True),("abbb",True),("abc",True),("abcb",True),("abd",True),("abdb",True)]
		sigma = "abcd"

bug0a = Tab s e w sigma
	where
		s = ["","c","cc"]
		e = ["c",""]
		w = [("",False),("a",False),("b",False),("c",False),("cc",True),("ca",False),("cb",False),("cca",True),("ccb",False),("ccc",False),("ac",False),("bc",False),("cac",False),("cbc",True),("ccac",False),("ccbc",False),("cccc",False)]
		sigma = "abc"
		
bug0b = Tab s e w sigma
	where
		s = ["","c","cc","a","ab","abc"]
		e = ["c",""]
		w = [("",False),("a",False),("b",False),("c",False),("cc",True),("ca",False),("cb",False),("cca",True),("ccb",False),("ccc",False),("ac",False),("bc",False),("cac",False),("cbc",True),("ccac",False),("ccbc",False),("cccc",False),("abc",True),("ab",False),("abcc",False),("aac",False),("aa",False),("acc",False),("abac",False),("aba",False),("abbc",True),("abb",False),("abcac",False),("abca",True),("abcbc",False),("abcb",False),("abccc",False)]
		sigma = "abc"

bug1b = Tab s e w sigma
	where
		s = ["","L","LO","LOL","O","OL","OLO","OLOL"]
		e = ["OL","L",""]
		w = [("",False),("L",False),("O",False),("U",False),("LO",False),("LOL",True),("LL",False),("LU",False),("LOO",False),("LOU",False),("LOLL",False),("LOLO",False),("LOLU",False),("OL",False),("UL",False),("LLL",False),("LUL",True),("LOOL",False),("LOUL",False),("LOLLL",False),("LOLOL",False),("LOLUL",False),("OOL",False),("UOL",False),("LLOL",False),("LUOL",False),("LOOOL",False),("LOUOL",False),("LOLLOL",False),("LOLOOL",False),("LOLUOL",False),("OLOL",False),("OLL",False),("OLOOL",False),("OLO",False),("OLOLOL",False),("OLOLL",False),("OOOL",False),("OO",False),("OUOL",False),("OUL",False),("OU",False),("OLLOL",False),("OLLL",False),("OLUOL",False),("OLUL",False),("OLU",False),("OLOOOL",False),("OLOO",False),("OLOUOL",False),("OLOUL",False),("OLOU",False),("OLOLLOL",False),("OLOLLL",False),("OLOLOOL",False),("OLOLO",False),("OLOLUOL",False),("OLOLUL",False),("OLOLU",False)]
		sigma = "abc"

--TODO s.Ex_13
angluin = do
	alph <- getAlphabet
	let tab = Tab [""] [""] [] alph
	learn tab

learn (Tab s e words a) = do
	newWords <- getWords $ neededEntries (Tab s e words a)
	if null newWords then do
		let newS = isComplete $ Tab s e words a
		if null newS then do
			let newE = isConsistent $ Tab s e words a
			if null newE then do
				print (Tab s e words a)
				putStrLn "Does this DFA accept your language? If not, provide a counter example. Else type :q."
				line <- getLine
				case line of
					":q" -> putStrLn "Exiting language learning..."
					":p" -> do
						print s
						print e
						print words
					word -> learn (Tab (nub (s ++ inits word)) e words a)
			else
				learn (Tab s (head newE:e) words a)
		else
			learn (Tab (head newS:s) e words a)
	else
		learn (Tab s e (words++newWords) a)

			
instance Show Table where
	show = showAutomatonFromTable
	--TODO: showsPrec :: Int -> a -> ShowS
	--TODO: showList :: [a] -> ShowS

--this output is only correct if the table is complete and consistent
showAutomatonFromTable (Tab s e w a) =
	let states = getStates  [ (a, [ lookup (a++b) w | b <- e ]) | a <- s ] --this is the s01 from below. TODO: make this only computed ONCE per cycle
	in
		--TODO: make pretty
		"States:\n"
		++ intercalate ", " (map getStateName states)
		++ "\nInitial state:\n"
		++ getStateName ""
		++ "\nTransitions:\n"
		++ unlines [getStateName q ++'-':z:"->" ++ getStateName q' | q <- states, z <- a, q' <- states, and [lookup (q++z:ex) w == lookup (q'++ ex) w | ex <- e] ]
		++ "Final states:\n"
		++ intercalate ", " (map getStateName (filter (\state -> lookup state w == Just True) states))


--TODO: make pretty, use ´on´
getStates = map fst . nubBy (\(_ , y) (_, k) -> y == k)
--or better:
--instance Eq Prefix
--getStates s = group s

--supplies words that are in SE, but not yet in the lookup table WordsinL
neededEntries a = nub $ makeValid a ++ getPrefixes a--TODO: need nub?
--but ++ is evil

{- bugged versions:
--better:
neededEntries' a = nub $ concat $ map a [makeValid, getPrefixes]

--interestingly, this one needs an explicit type signature, see http://www.haskell.org/haskellwiki/Monomorphism_restriction
--neededEntries'' :: Table -> [String]
--neededEntries''  = nub . concat . flip map [makeValid, getPrefixes]
-}

--for the lulz:
neededEntries''' = nub . concat . zipWith ($) [makeValid, getPrefixes] . repeat

makeValid :: Table -> [String]
makeValid (Tab s e w _) =
	filter (`notElem` map fst w) [ a ++ b | a <- s, b <- e ]

getPrefixes :: Table -> [String]
getPrefixes (Tab s e w alph) =
	filter (`notElem` map fst w) [ a++z:b | a <- s, z <- alph, b <- e ]

--the amount of tokens is too damn high
neededEntries'''' (Tab s e w alph) = filter (`notElem` map fst w) [ a++z++b | a <- s, z <- "":group alph, b <- e ]

--better to add one entry at a time? something like this:
neededEntry = head.neededEntries

--Why do I need these two? I don't remember. :(
isValid :: Table -> Bool
isValid (Tab s e w _) =
	and [ a++b `elem` map fst w | a <- s, b <- e ]

hasAllPrefixes (Tab s e w alph) =
	isValid (Tab s e w alph) && and [ a++z:b `elem` map fst w | a <- s, z <- alph, b <- e ]


--apply only if every word in SE has an entry in w
--provides a word in SSigma which has no equivalent 0-1-sequence in S, or Nothing if there is no such word
--isComplete :: Table -> Maybe String
isComplete (Tab s e w alph) =
	let
		-- This generates the 0-1-sequences for the prefixes in S
		s01 = [ [ lookup (a++b) w | b <- e ] | a <- s ]
		-- generate 0-1-sequences for S*Alph and discard those which are already in s01
	in map fst $ filter (not.snd) [ (a++[z] , [ lookup (a++z:b) w | b <- e ] `elem` s01) | a <- s, z <- alph ]
	--TODO optimization: once dropped, no need to re-add entry until E is changed
--TODO: avoid multiple words with the same 0-1-sequence being added. (restore Maybe type ?)

isConsistent (Tab s e w alph) =
	let
		--fun s1 s2 = [ lookup (s1++b) w | b <- e ] == [ lookup (s2++b) w | b <- e ]
		--s01 = [ (a, [ lookup (a++b) w | b <- e ]) | a <- s ]
		--fun = compare `on` snd
		--states = group (map fst . sortBy (compare `on` snd) s01)
		--fix: sort ´on´ snd s
		sequences = map (`s01` (e,w)) s
	in
		--nub $ concatMap (\s -> [ z:b | z <- alph, b <- e, s1 <- s, s2 <- s, lookup (s1++z:b) w /= lookup (s2++z:b) w]) states
		[ z: b | (s1 , v1) <- sequences, (s2, v2) <- sequences, v1 == v2, z <- alph, b <- e, lookup (s1++z:b) w /= lookup (s2++z:b) w ]
		

{-
isConsistent' (Tab s e w alph) =
	let
		fun s1 s2 = [ lookup (s1++b) w | b <- e ] == [ lookup (s2++b) w | b <- e ]
		states = groupBy fun s
	in do
		--nub $ concatMap (\s -> [ z:b | z <- alph, b <- e, s1 <- s, s2 <- s, lookup (s1++z:b) w /= lookup (s2++z:b) w]) states
		print [ [ lookup (a++b) w | b <- e ] | a <- s ]
		print states
-}

s01 prefix (e,w) = ( prefix, [ lookup (prefix++b) w | b <- e ])

getAlphabet :: IO String
getAlphabet = do
	putStrLn ("Please Enter your alphabet as one sequence of letters. It must not contain the character '" ++ getRepresentation "" ++ "'.")
	alph <- getLine
	if any (`elem` alph) $ getRepresentation "" then getAlphabet else return alph

--TODO: make pretty or replace with map (or >>=)
--getWords :: [String] -> IO [Bool]
getWords [] = return []
getWords (w:ws) = do
	b <- wordQuery w
	bs <- getWords ws
	return ((w,b): bs)

wordQuery w = do
	putStrLn $ "Is the word w in the language? Type y or n. w = " ++ getRepresentation w
	a <- getLine
	case a of
		"y" -> return True
		"n" -> return False
		_ -> wordQuery w

getRepresentation "" = "1"
getRepresentation w = w

getStateName w = '[':getRepresentation w++"]"

--TODO optimization: add ~