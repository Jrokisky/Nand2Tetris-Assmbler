import System.Environment
import Data.Maybe
import Control.Monad
import Data.List
import Text.Read
import Numeric (showIntAtBase)
import Data.Char (intToDigit, isDigit)
import SymbolTable


type Line = (Int, Maybe String) --original line number in raw file

destL = [("M", "001")
      , ("D", "010")
      , ("MD", "011")
      , ("A", "100")
      , ("AM", "101")
      , ("AD", "110")
      , ("AMD", "111")
      , ("", "000")]

jumpL = [("JGT", "001")
      , ("JEQ", "010")
      , ("JGE", "011")
      , ("JLT", "100")
      , ("JNE", "101")
      , ("JLE", "110")
      , ("JMP", "111")
      , ("", "000")]

compL = [("0", "0101010")
      , ("1", "0111111")
      , ("-1", "0111010")
      , ("D", "0001100")
      , ("A", "0110000")
      , ("!D", "0001101")
      , ("!A", "0110001")
      , ("-D", "0001111")
      , ("-A", "0110011")
      , ("D+1", "0011111")
      , ("A+1", "0110111")
      , ("D-1", "0001110")
      , ("A-1", "0110010")
      , ("D+A", "0000010")
      , ("D-A", "0010011")
      , ("A-D", "0000111")
      , ("D&A", "0000000")
      , ("D|A", "0010101")
      , ("M", "1110000")
      , ("!M", "1110001")
      , ("-M", "1110011")
      , ("M+1", "1110111")
      , ("M-1", "1110010")
      , ("D+M", "1000010")
      , ("D-M", "1010011")
      , ("M-D", "1000111")
      , ("D&M", "1000000")
      , ("D|M", "1010101")]


main = do 
    [fileName]      <- getArgs
    file            <- readFile fileName
    let toLine = map (\(x,y) -> (x, removeWhiteSpaceAndComments y)) 
               . filter (\(x,y) -> not . isComment $ y) -- remove comments
               . filter (\(x,y) -> y /= "")   -- remove newlines
               . filter (\(x,y) -> y /= "\r")
               . zip [1..] -- include raw line number for error reporting
               . lines 
               $ file
    let symbolTable = addLoopsToSymTable (snd . unzip $ toLine) 0 (Just symTab)
    let loopsRemoved = filter (\(x,y) -> not . isLoop $ y) toLine
    let nowLine = map (\(x,y) -> (x,Just y)) loopsRemoved
    let symsReplaced = replaceVariables nowLine (fromJust symbolTable)
    let binaryRep = map ( \(x,y) -> (x, (toBinary y))) symsReplaced
    let out = errorCheck binaryRep
    writeFile (changeSuffix fileName) out



-- Scan through output for Nothing. If found, there is an error on that line
errorCheck :: [Line] -> String
errorCheck ls | errors == [] =  unlines . map fromJust . snd . unzip $ ls
              | otherwise   = "Parse Errors on Lines:" 
                             ++ (foldr (++) "" (map ((++) " " . show . fst) errors))
          where 
            errors = filter (\(x,y) -> isNothing y) ls

--Check if a line is a comment
isComment :: String -> Bool
isComment (x:y:_) = x == '/' && y == '/'
isComment _       = False

-- Recursively replace variables w/ values from symbol table OR add vars to symbol table
replaceVariables :: [Line] -> SymTable -> [Line]
replaceVariables  [] _ = [] 
replaceVariables (x:xs) symTab
                | isSymbol (fromJust . snd $ x)  =  (fst x, value) : replaceVariables xs updatedSymTab
                | otherwise   = x : replaceVariables xs symTab
                        where
                           updatedSymTab = addSymbol (tail . fromJust . snd $ x) symTab
                           value = liftM2 (++) (Just "@") (findValue (tail .fromJust . snd $ x) updatedSymTab)

-- addsSymbol to symbol Table if needed 
addSymbol :: String -> SymTable ->  SymTable
addSymbol str symTab | contains str symTab = symTab
                     | otherwise           = addVar str symTab
                        

-- Checks whether an A instr that has a symbol
isSymbol :: String -> Bool
isSymbol str = head str == '@' && (not . isDigit . head . tail $ str)
                             

-- take a file name that ends in .asm, remove the .asm portion, & append .hack
changeSuffix :: String -> String
changeSuffix fileName = (reverse . snd . splitAt 4 . reverse $ fileName) ++ ".hack"

--Recursively remove (Loops) and add them to the symbol table
addLoopsToSymTable :: [String] -> Integer -> Maybe SymTable -> Maybe SymTable
addLoopsToSymTable _  _ Nothing = Nothing
addLoopsToSymTable [] _ fin@(Just symTab)  = fin
addLoopsToSymTable (x:xs) indx mayTab@(Just symTab) 
            | isLoop x = addLoopsToSymTable xs indx (addLoop (init . tail $ x) indx symTab)
            | otherwise  = addLoopsToSymTable xs (indx+1) mayTab

-- check if a string is a (LOOP)
isLoop :: String -> Bool
isLoop str | head str == '(' && last str == ')' = True
           | otherwise                         = False

-- remove all line whitespaces and comments
removeWhiteSpaceAndComments :: String -> String
removeWhiteSpaceAndComments str = line'''
         where 
            line  = filter (' ' /=) str
            line' = filter ('\r' /=) line
            line'' = filter ('\t' /=) line'
            line''' = removeComment line''



toBinary :: Maybe String ->  Maybe String
toBinary Nothing                                  = Nothing
toBinary (Just lineIn) 
         | head lineIn == '@'                       = parseAinstr (tail lineIn)
         | otherwise                              = parseCinstr lineIn


-- Remove comments at the end of a line
removeComment :: String -> String
removeComment ls = foldr (++) "" remCom
           where remCom = takeWhile (not . isPrefixOf "//") . group $ ls 

parseAinstr :: String -> Maybe String
parseAinstr aInstr =  liftM2 (++) prefix binary
                 where
                    binary = aToBinary (readMaybe aInstr :: Maybe Int)
                    prefix = calculateAinstrPrefix binary

calculateAinstrPrefix :: Maybe String -> Maybe String
calculateAinstrPrefix Nothing = Nothing 
calculateAinstrPrefix (Just a) = Just (replicate lenPre '0')
                         where 
                            lenPre = 16 - (length a)

aToBinary :: Maybe Int -> Maybe String
aToBinary Nothing = Nothing
aToBinary (Just a) | a < 0     = Nothing
                   | otherwise = Just (showIntAtBase 2 intToDigit a "")

parseCinstr :: String -> Maybe String
parseCinstr cInstr =  liftM2 (++) binary' jumpBinary
            where 
               (dest, cInstr')  = getDest cInstr  (elemIndex '=' cInstr) 
               (jump, comp)     = getJump cInstr' (elemIndex ';' cInstr')
               prefix           = Just "111"
               destBinary       = partToBinary dest destL
               jumpBinary       = partToBinary jump jumpL
               compBinary       = partToBinary comp compL
               binary           = liftM2 (++) prefix compBinary
               binary'          = liftM2 (++) binary destBinary
                   

-- Try to match give dest string to list of acceptable dest strings
partToBinary :: String -> [(String, String)] -> Maybe String
partToBinary partIn list | match == []   = Nothing 
                         | otherwise     = Just (snd . head $ match)
                     where match = filter ((==) partIn . fst) list

-- Takes a C Instr line and the Possible location of '='
-- Returns split at '=' 
getDest :: String -> Maybe Int -> (String, String)
getDest line (Just ind) = (fst split , tail . snd $ split) --tail drops =
             where split = splitAt ind line
getDest line Nothing    = ("", line)


-- Takes a C Instr line and the Possible location of ';'
-- Returns split at ';' 
getJump :: String -> Maybe Int -> (String, String)
getJump line (Just ind) = (snd split, init . fst $ split)
             where split = splitAt (ind+1) line
getJump line Nothing    = ("", line)



