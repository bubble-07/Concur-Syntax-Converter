import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State

iParse aParser source_name input = 
    runIndent source_name $ runParserT aParser () source_name input

data Expr = LispSymbol String |
            LispList [Expr] |
            SquareList [Expr] |
            CurlyList [Expr] |
            LispNumber String |
            LispString String

spaceseparated a = concat (map ((++ " ") . show) a)

instance Show Expr where
    show (LispSymbol a) = a
    show (LispList a) = "(" ++ spaceseparated a  ++ ")"
    show (SquareList a) = "[" ++ spaceseparated a ++ "]"
    show (CurlyList a) = "{" ++ spaceseparated a ++ "}"
    show (LispNumber a) = a
    show (LispString a) = "\"" ++ a ++ "\""

{- Optional part of the number (decimal point, digits, type) -}
parseNumberopt = do string "."
                    digitstring <- many1 digit
                    digittype <- option "" (string "f" <|> string "d")
                    return ("." ++ digitstring ++ digittype)

parseNumber = do firstpart <- many1 digit
                 secondpart <- option "" parseNumberopt
                 return $ LispNumber (firstpart ++ secondpart)

parseString = do
    result <- between (string "\"") (string "\"") (many (noneOf "\""))
    return $ LispString result

listcontent = (many space) >> (sepEndBy listitem (many1 (try parseComment <|> (many1 space))))

-- should "return" a list of Expr's
linelistcontentblockcase =
    do
        lookAhead (many (noneOf "\n") >> spaces >> indented)
        result <- parseBlockwocontainer
        spaces
        return result
linelistcontentnormcase = 
    do
        result <- sepBy1 listitem (many1 (try parseComment <|> (many1 (char ' '))))
        spaces
        return result
linelistcontent = (try linelistcontentblockcase) <|> linelistcontentnormcase


{- Parses something that is "list-like", between a and b into wrap-}
listlike a b wrap = do result <- between (string a) (string b) listcontent
                       return $ wrap result

listexpr = listlike "(" ")" LispList
squareexpr = listlike "[" "]" SquareList
curlyexpr = listlike "{" "}" CurlyList

{- "Fixed" function syntax is f(a b, c, d) -> (f (a b) c d) -}
parseFixed = do function <- symbol
                arguments <- between (string "(") (string ")") parseFixedargs
                return $ LispList ([function] ++ arguments)
parseFixedargs = 
    do
        result <- sepBy listcontent (string ",")
        return $ map LispList result

sepBy2 p s =
    do
        firstitem <- p
        s
        therest <- sepBy1 p s
        return (firstitem:therest)

{- "Object" syntax is like o.x.y -> (y (x (o)), but anything permissible in a list
can take the places of o, x, and y. No whitespace between, numbers or object syntax things
though! -}

parseObject = do listofthings <- sepBy2 objectitem (string ".")
                 return $ formatObject listofthings
formatObject listofobjects = formatObjectaux (reverse listofobjects)
formatObjectaux :: [Expr] -> Expr
formatObjectaux (x:[]) = x
formatObjectaux (x:xs) = LispList (x:[formatObjectaux xs])

parseComment = (try ((string "#") >> manyTill (noneOf "\n") (try (lookAhead newline)))) <|>
               (try ((string "/*") >> manyTill anyChar (string "*/")))

objectitem = (try parseFixed) <|> (try parseString) <|> (try symbol) <|> (try listexpr) <|>
             (try squareexpr) <|> (try curlyexpr)

listitem = (try parseNumber) <|> (try parseObject) <|> (try parseFixed) <|> 
           (try symbol) <|> (try parseString) <|>
           (try listexpr) <|>
           (try squareexpr) <|> (try curlyexpr)

symbol = do
            result <- (many1 symbolchars)
            return $ LispSymbol result
symbolchars = noneOf ".() ,\n\t{}[]#\""

{- "Block" syntax is of the form
f a b
    c      -> (f (a b) (c) (d e))
    d e
or
f
    a      -> (f (a) (b))
    b 
-}

{- combineBlockparts takes the header and the rest -}
combineBlockparts :: [Expr] -> [[Expr]] -> [Expr]
combineBlockparts (x:[]) rest = (x:(map LispList rest))
combineBlockparts (x:xs) rest = (x:(LispList xs):(map LispList rest))

parseBlockwocontainer = withBlock combineBlockparts linelistcontentnormcase linelistcontent
parseBlock = do result <- (many parseBlockwocontainer)
                return $ LispList (map LispList result)

-- (x) -> x  . Eliminates singleton lists
filtersingletons (LispList (x:[])) = filtersingletons x
filtersingletons (LispList xs) = LispList (map filtersingletons xs)
filtersingletons x = x

parseLisp :: String -> Either ParseError Expr
parseLisp input = iParse parseBlock "(unknown)" input
formatparseoutput (Left e) = "You made a silly: " ++ (show e)
formatparseoutput (Right e) = show (filtersingletons e)

parsefromstring :: String -> String
parsefromstring s = formatparseoutput (parseLisp s)

main = interact parsefromstring
