module SymbolTable where


type Table = [(String, String)]
type SymTable = (Integer, Table) --The first Int represents the next available memory address

symTab = (16, [ ("SP", "0")
             , ("LCL", "1")
             , ("ARG", "2")
             , ("THIS", "3")
             , ("THAT", "4")
             , ("SCREEN", "16384")
             , ("KBD", "24576")
             , ("R0", "0")
             , ("R1", "1")
             , ("R2", "2")
             , ("R3", "3")
             , ("R4", "4")
             , ("R5", "5")
             , ("R6", "6")
             , ("R7", "7")
             , ("R8", "8")
             , ("R9", "9")
             , ("R10", "10")
             , ("R11", "11")
             , ("R12", "12")
             , ("R13", "13")
             , ("R14", "14")
             , ("R15", "15")])

findValue :: String -> SymTable -> Maybe String
findValue str tab  | find == []  = Nothing
                   | otherwise   = Just . snd . head $ find
          where find = filter (\(x,y) -> x == str) (snd tab)


contains :: String -> SymTable -> Bool
contains str tab | find == []  = False
                 | otherwise   = True
          where find = filter (\(x,y) -> x == str) (snd tab)



addVar :: String -> SymTable -> SymTable
addVar str tab  = (nextMem + 1, (table ++ [(str, show nextMem)]))
               where 
                   nextMem = fst tab
                   table   = snd tab


addLoop :: String -> Integer -> SymTable -> Maybe SymTable
addLoop str line tab | contains str tab = Nothing
                     | otherwise        = Just (nextMem, (table ++ [(str, show line)]))
                  where 
                     nextMem = fst tab
                     table   = snd tab
