module DumpTree
    ( dump
    ) where

import Debug.Trace
import System.Exit
import Data.List
import Lib

-- DUMP ELEMENT NAME
dumpName :: String -> IO()
dumpName name = putStr ("<xs:element name=" ++ show name)

-- CHOOSE BEST TYPE
chooseType :: String -> String -> String
chooseType [] [] = []
chooseType str [] = str
chooseType [] str = str
chooseType "xs:string" _ = "xs:string"
chooseType _ "xs:string" = "xs:string"
chooseType "xs:positiveInteger" "xs:negativeInteger" = "xs:decimal"
chooseType "xs:negativeInteger" "xs:positiveInteger" = "xs:decimal"
chooseType "xs:positiveInteger" "xs:decimal" = "xs:decimal"
chooseType "xs:decimal" "xs:positiveInteger" = "xs:decimal"
chooseType "xs:decimal" "xs:negativeInteger" = "xs:decimal"
chooseType "xs:negativeInteger" "xs:decimal" = "xs:decimal"
chooseType str str' = str

-- DUMP THE VALUE
dumpValue :: String -> IO()
dumpValue [] = return ()
dumpValue value = putStr (" type=" ++ show value)

-- DUMP MAX AND MIN OCCUR
dumpMaxMin :: [Tree String] -> [[Tree String]] -> (Int, Int) -> IO()
dumpMaxMin [] [] (_, _) = return ()
dumpMaxMin current [] (_, 1) = return ()
dumpMaxMin current [] (_, max) = putStr " maxOccurs=\"unbounded\""
dumpMaxMin current sequences (min, max) = do
      if min /= 1 then putStr (" minOccurs=" ++ show min) else putStr ""
      if max /= 1 then putStr (" maxOccurs=" ++ show max) else putStr ""

-- DUMP ALL ATTRIBUTS
dumpAttributs :: [Attribut String] -> IO()
dumpAttributs [] = return ()
dumpAttributs ((Attribut name value):xs) = do
      putStrLn ("<xs:attribute name=" ++ show name ++ " type=\"xs:string\"/>")
      dumpAttributs xs

-- RETURN ALL ATTRIBUT WITH THE SAME NAME
findAllSameElem :: [Tree String] -> Tree String -> [Tree String]
findAllSameElem [] _ = []
findAllSameElem ((Node str value elems atts):tree) (Node name _ _ _) = if name == str then (Node str value elems atts) : findAllSameElem tree (Node name [] [] []) else findAllSameElem tree (Node name [] [] [])

-- RETURN A LIST OF UNIQUE ATTRIBUT
removeSameElem :: [Tree String] -> Tree String -> [Tree String]
removeSameElem [] _ = []
removeSameElem ((Node str value elems atts):tree) (Node name _ _ _) = if (name /= str) then (Node str value elems atts) : removeSameElem tree (Node name [] [] []) else removeSameElem tree (Node name [] [] [])

-- RETURN THE GOOD TYPE OF THE VALUE
findValueType :: [Tree String] -> String
findValueType [] = []
findValueType ((Node _ value _ _):xs) = chooseType value (findValueType xs)

-- FIND ALL ELEMENTS IN SEQUENCES
findElems :: [Tree String] -> [Tree String]
findElems [] = []
findElems ((Node name value [] atts):xs) = []
findElems ((Node name value elems atts):xs) = elems ++ (findElems xs)

-- FIND AN ATTRIBUT IN A LIST
sameAttributs :: [Attribut String] -> String -> Bool
sameAttributs [] _ = False
sameAttributs ((Attribut name _):xs) name' = if name == name' then True else sameAttributs xs name'

-- REMOVE SAME ATTRIBUT WITH A NAME
removeSAttsBase :: [Attribut String] -> String -> [Attribut String]
removeSAttsBase [] _ = []
removeSAttsBase ((Attribut name' val):xs) name = if name == name' then removeSAttsBase xs name else (Attribut name' val) : removeSAttsBase xs name

-- REMOVE SAME ATTRIBUT
removeSameAttributs :: [Attribut String] -> [Attribut String]
removeSameAttributs [] = []
removeSameAttributs ((Attribut name val):xs) = (Attribut name val) : (removeSameAttributs (removeSAttsBase xs name))

-- FIND ALL ATTRIBUT LINKED TO A ELEMENT
findAttributs :: [Tree String] -> [Attribut String]
findAttributs [] = []
findAttributs ((Node _ _ _ attributs):xs) = attributs ++ (findAttributs xs)

-- FIND ELEMENT NAME
findName :: [Tree String] -> String
findName [] = []
findName ((Node name _ _ _):xs) = name

-- CREATE A NEW ELEMENT FROM A SEQUENCE OF SAME ELEMENTS
createElementFT :: [Tree String] -> Tree String
createElementFT xs = Node (findName xs) (findValueType xs) (findElems xs) (removeSameAttributs (findAttributs xs))

-- CREATE AN ELEMENT FROM THE ACTUAL ELEM + SAME ELEM IN THE SAME SEQUENCE + SAME ELEM IN DIFFERENTS SEQUENCE
createElement :: Tree String -> [Tree String] -> [[Tree String]] -> (Tree String, [Tree String], [[Tree String]])
createElement elem [] [] = (elem, [], [])
createElement elem current [] = (createElementFT (elem:current), current, [])
createElement elem current sequences = (createElementFT ((elem:current) ++(concat sequences)), current, sequences)

-- RETURN ALL SAME ELEM IN A SEQUENCE
getAllElementBySequence :: String -> [Tree String] -> [Tree String]
getAllElementBySequence name [] = []
getAllElementBySequence name ((Node name' value elems attributs):xs) = if name == name' then (Node name' value elems attributs) : getAllElementBySequence name xs else getAllElementBySequence name xs

-- RETURN A LIST OF SEQUENCE OF SAME ELEM
transformInSeq :: Tree String -> [Tree String] -> [[Tree String]]
transformInSeq _ [] = []
transformInSeq (Node name _ _ _) ((Node name' value elems attributs):xs) =  (getAllElementBySequence name elems) : transformInSeq (Node name [] [] []) xs

-- DEBUG
pf :: [Tree String] -> IO()
pf [] = return ()
pf ((Node name value elems att):xs) = do
      putStrLn "#~~~~~#"
      putStrLn ("name = " ++ show name ++ " value= " ++ show value)
      if length elems > 0 then putStrLn "ELEMS REMPLI" else putStrLn "NOTHING IN ELEM"
      putStrLn "#~~~~~#"
      pf xs

-- DEBUG
pt :: [[Tree String]] -> Int -> IO()
pt [] _ = return ()
pt (x:xs) i = do
      print i
      pf x
      pt xs (i +1)

-- DUMP AN ELEMENT
dumpElements :: [Tree String] -> [Tree String] -> IO()
dumpElements [] _ = return ()
dumpElements (node:sequence) otherseq = do
      let seqs = transformInSeq node otherseq
      let (element, current, others) = createElement node (findAllSameElem sequence node) seqs
      dumpElem element current others
      dumpElements (removeSameElem sequence node) otherseq


-- FIND MAX ELEM IN A SEQUENCE
findMaxElements :: [Tree String] -> Int -> Int
findMaxElements [] max = max
findMaxElements ((Node _ _ elems _):xs) i = if len >= i then findMaxElements xs len else findMaxElements xs i where
                        len = length elems

-- DUMP THE COMPLEX TYPE OF AN ELEM
dumpComplexType :: [Tree String] -> [Attribut String] -> [Tree String] -> IO()
dumpComplexType current attributs others = do
      putStrLn "<xs:complexType>"
      let max = findMaxElements others 0
      if length current > 1 || max > 1 then putStrLn "<xs:sequence>" else putStr ""
      dumpElements current others
      if length current > 1 || max > 1 then putStrLn "</xs:sequence>" else putStr ""
      dumpAttributs attributs
      putStrLn "</xs:complexType>"


-- FIND MIN / MAX OCCUR OF AN ELEMENT IN A SEQUENCE
findMinMax :: [[Tree String]] -> (Int, Int) -> (Int, Int)
findMinMax [] (min, max) = (min, max)
findMinMax (x:xs) (mi, ma) = do
                        let i = length x
                        findMinMax xs (min i mi, max i ma)


-- DUMP AN ELEM
dumpElem :: Tree String -> [Tree String] -> [[Tree String]] -> IO()
dumpElem (Node name value elems attributs) current sequences = do
      dumpName name
      dumpValue value
      dumpMaxMin current sequences $ findMinMax (((Empty):current):sequences) (99999999999, 0)
      if (value == []) then putStrLn ">" else putStrLn "/>"
      if (length elems == 0 && length attributs == 0) then putStr "" else dumpComplexType elems attributs current
      if (value /= []) then putStr "" else putStrLn "</xs:element>"

-- DUMP THE WHOLE TREE
dump :: (Tree String, String, Res) -> IO()
dump (_, _, ErrorParse) = exitWith $ ExitFailure 84
dump (Empty, _, res) = putStrLn ""
dump (tree, _, Parsed) = do
      putStrLn "<?xml version=\"1.0\" encoding=\"UTF -8\"?>"
      putStrLn "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">"
      putStrLn ""
      dumpElem tree [] []
      putStrLn "</xs:schema>"

