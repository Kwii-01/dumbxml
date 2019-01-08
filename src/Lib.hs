module Lib
    ( Tree(..),
      Attribut(..),
      Res(..),
      parser
    ) where

import Debug.Trace
import Data.List
import Data.Bool
import Text.Read
import System.Exit

data Attribut a = Attribut String a
                | NoAttribut

data Res = Parsed
        | ErrorParse

data Tree a = Empty
            | Node a a [Tree a] [Attribut a]
            --deriving (Eq,Ord,Show,Read)


-- TOOL FUNCTIONS
strncomp :: String -> String -> Int -> Bool
strncomp [] [] 0 = True
strncomp _ [] _ = False
strncomp _ _ 0 = True
strncomp (x:xs) (t:ts) i = if x == t then strncomp xs ts (i - 1) else False

splitThis :: String -> Char -> Maybe (String, String)
splitThis str c = case elemIndex c str of
                    Just i -> Just $ splitAt i str
                    Nothing -> Nothing

split :: String -> Char -> (String, String, Res)
split str c = case splitThis str c of
                Just (s1, x:s2) -> (s1, s2, Parsed)
                Nothing -> ([], [], ErrorParse)

nextW :: String -> String
nextW [] = []
nextW (' ':xs) = nextW xs
nextW ('\t':xs) = nextW xs
nextW ('\n':xs) = nextW xs
nextW xs = xs


-- PARSE ELEM NAME
getName :: String -> (String, String, Res)
getName [] = ([], [], ErrorParse)
getName (' ':xs) = ([], (nextW xs), Parsed)
getName ('>':xs) = ([], '>':xs, Parsed)
getName ('<':xs) = ([], [], ErrorParse)
getName (s:xs) = let
        (name, after, res) = getName xs
        in (s:name, after, res)

pElemName :: String -> (String, String, Res)
pElemName (x:xs) = case x of
                    '<' -> getName xs
                    _ -> ([], [], ErrorParse)


-- PARSE ATTRIBUTS
getAttributValue :: String -> (String, String, Res)
getAttributValue [] = ([], [], ErrorParse)
getAttributValue ('\"':xs) = split xs '\"'
getAttributValue xs = ([], [], ErrorParse)

getAttribut :: String -> (Attribut String, String, Res)
getAttribut str = attr where
            (name, s, res1) = split str '='
            attr = case res1 of
                    ErrorParse -> (NoAttribut, [], ErrorParse)
                    _ -> final
            
            final = case getAttributValue s of
                        ([], [], ErrorParse) -> (NoAttribut, [], ErrorParse)
                        (value, snext, res) -> (Attribut name "xs:string", snext, res)

pElemAttributs :: String -> ([Attribut String], String, Res)
pElemAttributs [] = ([], [], ErrorParse)
pElemAttributs ('>':xs) = ([], nextW xs, Parsed)
pElemAttributs xs = attlist where
                (attribut, snext, res) = getAttribut xs

                attlist = case res of
                            ErrorParse -> ([], [], ErrorParse)
                            _ -> continue
                continue = case pElemAttributs $ nextW snext of
                            ([], [], ErrorParse) -> ([], [], ErrorParse)
                            (attributs, final, res1) -> (attribut : attributs, final, res1)


-- PARSE END BALISE
pEndBaliseComp :: String -> String -> Bool
pEndBaliseComp _ [] = False
pEndBaliseComp [] ('>':xs) = True
pEndBaliseComp [] xs = False
pEndBaliseComp (x:xs) (t:ts) = if x == t then pEndBaliseComp xs (nextW ts) else False

pEndBc :: String -> String -> Bool
pEndBc _ [] = False
pEndBc [] xs = False
pEndBc xs ('<':'/':ts) = pEndBaliseComp xs ts
pEndBc xs ts = False


-- PARSE ALL ELEMENTS
pElementAllElem :: String -> String -> ([Tree String], String, Res)
pElementAllElem [] _ = ([], [], ErrorParse)
pElementAllElem ('<':xs) name = elemlist where
                            (tree, s, res) = pElement ('<':xs)

                            elemlist = case res of
                                        ErrorParse -> ([], [], ErrorParse)
                                        _ ->
                                            case pEndBc name (nextW s) of
                                                True -> ([tree], (nextW s), Parsed)
                                                False -> final

                            final = case pElementAllElem (nextW s) name of
                                        ([], [], ErrorParse) -> ([], [], ErrorParse)
                                        (trees, s', res) -> (tree : trees, s', res)
pElementAllElem xs _ = ([], xs, Parsed)


-- PARSE VALUE OF ELEMENT
pElemValue :: String -> String -> (String, String, Res)
pElemValue [] _ = ([], [], ErrorParse)
pElemValue ('<':xs) name = value where
                        value = case pEndBc name ('<':xs) of
                                    True -> ([], ('<':xs), Parsed)
                                    False -> ([], [], ErrorParse)
pElemValue (x:xs) name = value where
                    value = case pElemValue xs name of
                        ([], [], ErrorParse) -> ([], [], ErrorParse)
                        (val, s, Parsed) -> (x : val, s, Parsed)

pValueType :: String -> String
pValueType [] = []
pValueType value = ftype where
                ftype = case (readMaybe value :: Maybe Int) of
                            Just i -> if i < 0 then "xs:negativeInteger" else "xs:positiveInteger"
                            Nothing -> dtype
                
                dtype = case (readMaybe value :: Maybe Double) of
                    Just i -> "xs:decimal"
                    Nothing -> "xs:string"

-- remove a balise
dropBalise :: String -> String
dropBalise str = s where
            s = case elemIndex '>' str of
                    Just i -> drop (i + 1) str
                    _ -> []

-- PARSE ELEMENT
pElement :: String -> (Tree String, String, Res)
pElement str = element where
            (name, s1, res1) = pElemName $ nextW str
            (atts, s2, res2) = pElemAttributs s1
            (elems, s3, res3) = pElementAllElem s2 name
            (value, s4, res4) = pElemValue (nextW s3) name

            element = case res1 of
                        ErrorParse -> (Empty, [], ErrorParse)
                        _ ->
                            case res2 of
                                ErrorParse -> (Empty, [], ErrorParse)
                                _ ->
                                    case res3 of
                                        ErrorParse -> (Empty, [], ErrorParse)
                                        _ -> case res4 of
                                            ErrorParse -> (Empty, [], ErrorParse)
                                            _ -> (Node name (pValueType value) elems atts, (nextW (dropBalise s4)), Parsed)


-- PARSE XML DECLARATION
xmlVersion :: String
xmlVersion = "version=\"1.0\" "

xmlEncoding :: String
xmlEncoding = "encoding=\"UTF -8\""

pDeclAtt :: String -> String -> (Res, String)
pDeclAtt [] _ = (ErrorParse, [])
pDeclAtt str att = case stripPrefix att str of
                    Just res -> (Parsed, res)
                    Nothing -> (ErrorParse, [])

pDecl :: String -> (Res, String)
pDecl str = case pDeclAtt (nextW str) "<?xml " of
                    (ErrorParse, []) -> (ErrorParse, [])
                    (Parsed, res1) -> 
                        case pDeclAtt (nextW res1) xmlVersion of
                            (ErrorParse, []) -> (ErrorParse, [])
                            (Parsed, res2) ->
                                case pDeclAtt (nextW res2) xmlEncoding of
                                    (ErrorParse, []) -> (ErrorParse, [])
                                    (Parsed, res3) ->
                                        case pDeclAtt (nextW res3) "?>" of
                                            (ErrorParse, []) -> (ErrorParse, [])
                                            (Parsed, final) -> (Parsed, final)



parser :: String -> (Tree String, String, Res)
parser [] = (Empty, [], ErrorParse)
parser str = parsing where
    (res1, st) = pDecl str
    (tree, string, res) = pElement st

    parsing = case res1 of
                ErrorParse -> (Empty, [], ErrorParse)
                _ ->
                    case res of
                        ErrorParse -> (Empty, [], ErrorParse)
                        _ ->
                            case string of
                                [] -> (tree, [], Parsed)
                                _ -> (Empty, [], ErrorParse)
