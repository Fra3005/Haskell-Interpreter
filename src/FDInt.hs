{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module FDInt(logo) where

import Control.Applicative
import Data.Char
import System.IO
import Debug.Trace
import Data.List


--Defining of type Parser
newtype Parser a = P (Env -> String -> [(Env, a, String)])

parse :: Parser a -> Env -> String -> [(Env, a, String)]
parse (P p) env inp = p env inp

--Defining of FUNCTOR, APPLICATIVE ,MONAD and ALTERNATIVE

instance Functor Parser where
  fmap g p =
    P
      ( \env inp -> case parse p env inp of
          [] -> []
          [(env, v, out)] -> [(env, g v, out)]
      )

instance Applicative Parser where
  pure v = P (\env inp -> [(env, v, inp)])
  pg <*> px =
    P
      ( \env inp -> case parse pg env inp of
          [] -> []
          [(env, g, out)] -> parse (fmap g px) env out
      )

instance Monad Parser where
  return v = P (\env inp -> [(env, v, inp)])
  p >>= f =
    P
      ( \env inp -> case parse p env inp of
          [] -> []
          [(env, v, out)] -> parse (f v) env out
      )

instance Alternative Parser where
  empty = P (\env inp -> [])
  p <|> q =
    P
      ( \env inp -> case parse p env inp of
          [] -> parse q env inp
          [(env, v, out)] -> [(env, v, out)]
      )

--Defining the type of a single object Env
data Variable = Variable
  { varName :: String,
    varType :: String,
    varValue :: Int
  }
  deriving (Show)

--Defining of a list of Variable
type Env = [Variable]

--Definition of Environment Management
updateEnv :: Variable -> Parser String
updateEnv var =
  P
    ( \env input -> case input of
        xs -> [((modifyEnv env var), "", xs)]
    )


modifyEnv :: Env -> Variable -> Env
modifyEnv [] var = [var]
modifyEnv (x : xs) newVariable =
  if (varName x) == (varName newVariable)
    then [newVariable] ++ xs
    else [x] ++ modifyEnv xs newVariable


--Function that read the variable given a varName
readVar :: String -> Parser Int
readVar varName =
  P
    ( \env input -> case searchVar env varName of
        [] -> []
        [varValue] -> [(env, varValue, input)]
    )


--Function that find the value associated of a given name in the Env
searchVar :: Env -> String -> [Int]
searchVar [] name = []
searchVar (x : xs) name =
  if (varName x) == name
    then [(varValue x)]
    else searchVar xs name


--Function to manage the array structure
saveArray :: String ->[Int] -> Parser String
saveArray var val = P(\env input -> [(updateArray env var val, "", input)])


updateArray :: Env -> String -> [Int] -> Env
updateArray env var val = foldl (modifyEnv) env l
                          where l = zipWith (\a i ->
                                    Variable {varName=var ++ "{" ++ (show i) ++ "}",
                                              varType ="array",
                                              varValue = a}) val [0..]


searchArray :: Env -> String -> [Int]
searchArray env array = 
    case searchVar env x of
      [] -> []
      value -> concat ([value] ++ map (\var -> searchVar env var) xs)
    where (x:xs) = map (\i -> (array ++ "{" ++ (show i) ++ "}")) [0..l]
          l = countElem env
          countElem [] = 0
          countElem (x:xs) = if (array ++ "{") `isPrefixOf` (varName x)
                                then 1 + countElem xs
                                else countElem xs


readArray :: String -> Parser [Int]
readArray name = 
  P
    ( \env input -> case searchArray env name of
        []->[]
        value -> [(env, value, input)]
    )

-------------------------------------------

--Simple Parser operation
item :: Parser Char
item =
  P
    ( \env input -> case input of
        [] -> []
        (x : xs) -> [(env, x, xs)]
    )

--Check if x satisfy a property given as input
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x
    then return x
    else empty

--Chek if a given input is digit and trasform it
digit :: Parser Char
digit = sat isDigit

--Check if a char is lowercase
lower :: Parser Char
lower = sat isLower

--Check if a char is uppercase
upper :: Parser Char
upper = sat isUpper

--Check if a value is alpha
isLetter :: Parser Char
isLetter = sat isAlpha

--Check if a value is alphanum
alphaNum :: Parser Char
alphaNum = sat isAlphaNum

--Return the identifier in which the first letter is lowerCase
identifier :: Parser String
identifier = do
  x <- lower
  xs <- many alphaNum
  return (x : xs)

--Return the list of symbol inserted
symbol :: String -> Parser String
symbol [] = return ""
symbol (x : xs) = do
  sat (x ==)
  symbol xs
  return (x : xs)

--Function to check Natural
natural :: Parser Int
natural = do
  xs <- some digit
  return (read xs)

--heck if a digit is in number form ("- or +") sing
integer :: Parser Int
integer =
  do
    symbol "-"
    n <- natural
    return (- n)
    <|> natural

------------------------------------------------------------------------
--Defining Arithmetic expression
aexp :: Parser Int
aexp =
  ( do
      t <- aterm
      symbol "+"
      e <- aexp
      return (t + e)
  )
    <|> do
      t <- aterm
      symbol "-"
      e <- aexp
      return (t - e)
    <|> aterm

aterm :: Parser Int
aterm = do
  f <- factor
  t <- aterm2 f
  return (t)

factor :: Parser Int
factor =
  do
    symbol "("
    e <- aexp
    symbol ")"
    return e
    <|> do
      i <- identifier
      readVar i
    <|> do 
      i<-identifier
      symbol "{"
      index <- aexp
      symbol "}"
      readVar $ i ++ "{" ++ (show index) ++ "}"
    <|> integer

aterm2 :: Int -> Parser Int
aterm2 t1 =
  do
    symbol "*"
    f <- factor
    t <- aterm2 (t1 * f)
    return (t)
    <|> do
      symbol "/"
      f <- factor
      t <- aterm2 (div t1 f)
      return (t)
    <|> return t1

--Define Parser for arithmetic expression
parseFact :: Parser String
parseFact =
  do
    symbol "("
    e <- parseAexp
    symbol ")"
    return ("(" ++ e ++ ")")
    <|> do
      symbol "-"
      f <- parseFact
      return ("-" ++ f)
    <|> do
      i <- identifier
      symbol "{"
      index <- parseAexp
      symbol "}"
      return $ i ++ "{" ++ index ++ "}"
    <|> do
      i <- identifier
      return i
    <|> do
      i <- integer
      return (show i)

parseAexp :: Parser String
parseAexp =
  do
    t <- parseAterm
    symbol "+"
    e <- parseAexp
    return (t ++ "+" ++ e)
    <|> do
      t <- parseAterm
      symbol "-"
      e <- parseAexp
      return (t ++ "-" ++ e)
    <|> parseAterm

parseAterm :: Parser String
parseAterm = do
  f <- parseFact
  do
    symbol "*"
    t <- parseAterm
    return (f ++ "*" ++ t)
    <|> do
      symbol "/"
      t <- parseAterm
      return (f ++ "/" ++ t)
    <|> return f

-----------------------------------------------------------------------------
---Defining bolean expression
bexp :: Parser Bool
bexp =
  do
    bool <- bterm
    symbol " OR "
    bool1 <- bexp
    return (bool || bool1)
    <|> bterm

bterm :: Parser Bool
bterm =
  do
    boolf <- bfactor
    symbol " AND "
    bool <- bterm
    return (boolf && bool)
    <|> bfactor

bfactor :: Parser Bool
bfactor =
  do
    symbol "!"
    bool <- bfactor
    return $ not bool
    <|> do
      symbol "("
      bool <- bexp
      symbol ")"
      return bool
    <|> do
      symbol "True"
      return True
    <|> do
      symbol "False"
      return False
    <|> bcomparison

bcomparison :: Parser Bool
bcomparison =
  do
    a <- aexp
    symbol "=="
    b <- aexp
    return $ a == b
    <|> do
      a <- aexp
      symbol "<="
      b <- aexp
      return $ a <= b
    <|> do
      a <- aexp
      symbol ">="
      b <- aexp
      return $ a >= b
    <|> do
      a <- aexp
      symbol ">"
      b <- aexp
      return $ a > b
    <|> do
      a <- aexp
      symbol "<"
      b <- aexp
      return $ a < b
    <|> do
      a <- aexp
      symbol "!="
      b <- aexp
      return $ a /= b

--Parser boolean expression
parseBoolExp :: Parser String
parseBoolExp =
  do
    bool1 <- parseBoolTerm
    symbol " OR "
    bool2 <- parseBoolExp
    return (bool1 ++ " OR " ++ bool2)
    <|> do
      bool <- parseBoolTerm
      return bool

parseBoolTerm :: Parser String
parseBoolTerm =
  do
    bool1 <- parseBoolFactor
    symbol " AND "
    bool2 <- parseBoolTerm
    return (bool1 ++ " AND " ++ bool2)
    <|> do
      bool <- parseBoolFactor
      return bool

parseBoolFactor :: Parser String
parseBoolFactor =
  do
    symbol "("
    bool <- parseBoolExp
    symbol ")"
    return ("(" ++ bool ++ ")")
    <|> do
      symbol "True"
      return "True"
    <|> do
      symbol "False"
      return "False"
    <|> do
      symbol "!"
      p <- parseBoolFactor
      return ("!" ++ p)
    <|> do
      c <- parseCompareTo
      return c

--Compare Parser operations
parseCompareTo :: Parser String
parseCompareTo =
  do
    a1 <- parseAexp
    symbol "=="
    a2 <- parseAexp
    return (a1 ++ "==" ++ a2)
    <|> do
      a1 <- parseAexp
      symbol "<="
      a2 <- parseAexp
      return (a1 ++ "<=" ++ a2)
    <|> do
      a1 <- parseAexp
      symbol ">="
      a2 <- parseAexp
      return (a1 ++ ">=" ++ a2)
    <|> do
      a1 <- parseAexp
      symbol "<"
      a2 <- parseAexp
      return (a1 ++ "<" ++ a2)
    <|> do
      a1 <- parseAexp
      symbol ">"
      a2 <- parseAexp
      return (a1 ++ ">" ++ a2)
    <|> do
      a1 <- parseAexp
      symbol "!="
      a2 <- parseAexp
      return (a1 ++ "!=" ++ a2)

--Defining parser for Program
prog :: Parser String
prog =
  do
    command
    prog
    <|> do
      command

parseProgram :: Parser String
parseProgram =
  ( do
      x <- parseCommand

      y <- parseProgram
      return $ x ++ y
  )
    <|> ( do
            x <- parseCommand

            return $ x
        )

--Parser command
command :: Parser String
command = assignment <|> ifThenElse <|> whiledo <|>  symbol "skip" <|> ternary

parseCommand :: Parser String
parseCommand =
  parseAssignment <|> parseIfThenElse <|> parseWhileDo <|>  symbol "skip" <|> parseTernary

--Function of assignment
assignment :: Parser String
assignment =
  -- x=aexp
  do
    id <- identifier
    symbol "="
    a <- aexp
    symbol ";"
    updateEnv Variable {varName = id, varType = "int", varValue = a}
  <|> 
  -- y=x{1}
  do 
    id<- identifier
    symbol "="
    id1 <- identifier
    symbol "{"
    val <- aexp
    symbol "}"
    symbol ";"
    value <- readVar (id1 ++ "{" ++ (show val) ++ "}")
    updateEnv Variable{varName = id, varType="int", varValue=value}
  <|> 
  -- x = {1,2,3}
  do 
    id <- identifier
    symbol "="
    arr <- array
    symbol ";"
    saveArray id arr
  <|> 
  -- x{1} = y{1}
  do
    id <-identifier
    symbol "{"
    index <-aexp
    symbol "}"
    symbol "="
    id2 <- identifier
    symbol "{"
    index2 <-aexp
    symbol "}"
    symbol ";"
    val <- readVar(id2 ++ "{" ++(show index2) ++ "}")
    updateEnv Variable {varName = (id ++ "{" ++ (show index) ++ "}"), varType="array", varValue=val}
  <|> 
  -- x{1} = y
  do
    id <- identifier
    symbol "{"
    index <-aexp
    symbol "}"
    symbol "="
    val <-aexp
    symbol ";"
    array <- readArray id
    if length array<=index
      then empty
    else updateEnv Variable {varName = (id ++ "{" ++ show(index) ++ "}"), 
                             varType="array", 
                             varValue=val}
    <|> 
    -- x = y++z
    do
      id<-identifier
      symbol "="
      ar1 <- array
      symbol " <-> "
      ar2 <- array
      symbol ";"
      saveArray id(ar1 ++ ar2)

--Define parser for Assignment operations
parseAssignment :: Parser String
parseAssignment =
  do
    id <- identifier
    symbol "="
    a <- parseAexp
    symbol ";"
    return (id ++ "=" ++ a ++ ";")
  <|> do id <- identifier
         symbol "="
         arr <- parseArray
         symbol ";"
         return (id ++ "=" ++ arr ++ ";")
  <|> do id <- identifier
         symbol "="
         id2 <- identifier
         symbol "{"
         index <- parseAexp
         symbol "}"
         symbol ";"
         return (id ++ "=" ++ id2 ++ "{" ++ index ++ "}" ++ ";")

  <|> do id <- identifier
         symbol "{"
         index <- parseAexp
         symbol "}"
         symbol "="
         id2 <- identifier
         symbol "{"
         index2 <- parseAexp
         symbol "}"
         symbol ";"
         return (id ++ "{" ++ index ++ "}=" ++ id2 ++ "{" ++ index ++ "}" ++ ";" )
  <|> do id <- identifier
         symbol "{"
         index <- parseAexp
         symbol "}"
         symbol "="
         val <- parseAexp
         symbol ";"
         array <- readArray id 
         return (id ++ "{" ++ index ++ "}=" ++ val ++ ";")
  <|> do id <- identifier 
         symbol "="
         ar1 <- parseArray
         symbol "<->"
         ar2 <- parseArray
         symbol ";"
         return $ id ++ "=" ++ ar1 ++ " <-> " ++ ar2 ++ ";" 

--If Then Else operation
ifThenElse :: Parser String
ifThenElse =
  do
    symbol "if("
    b <- bexp
    symbol ")"
    symbol "then("
    if (b)
      then do
        prog
        symbol ")"
        symbol "else("
        parseProgram
        symbol ")"
        symbol "endif"
        return ""
      else do
        parseProgram
        symbol ")"
        symbol "else("
        prog
        symbol ")"
        symbol "endif"
        return ""

--Parser of IfThenElse
parseIfThenElse :: Parser String
parseIfThenElse = do
  symbol "if("
  b <- parseBoolExp
  symbol ")"
  symbol "then("
  x <- parseProgram
  symbol ")"
  symbol "else("
  y <- parseProgram
  symbol ")"
  symbol "endif"
  return $ "if(" ++ b ++ ")" ++ "then(" ++ x ++ ")" ++ "else(" ++ y ++ ")" ++ "endif"

--------------------------------------------------------------------------
executeWhile :: String -> Parser String
executeWhile c = P (\env input -> [(env, "", c ++ input)])


--Define Whiledo cycle
whiledo :: Parser String
whiledo = do
  p <- parseWhileDo
  executeWhile p
  symbol "while("
  b <- bexp
  symbol ")"
  symbol "do{"
  if (b)
    then do
      prog
      symbol "}"
      executeWhile p
      whiledo
    else do
      parseProgram
      symbol "}"
      return ""

--Parser Whiledo
parseWhileDo :: Parser String
parseWhileDo = do
  symbol "while("
  b <- parseBoolExp
  symbol ")"
  symbol "do{"
  x <- parseProgram
  symbol "}"
  return $ "while(" ++ b ++ ")" ++ "do{" ++ x ++ "}"

--Ternary operation
ternary :: Parser String
ternary = do
  symbol "("
  b <- bexp
  symbol ")"
  symbol "?"
  if(b)
    then
      do
        prog
        symbol " : "
        parseProgram
    else
      do
        parseProgram
        symbol " : "
        prog
        return ""

--Parser Ternary
parseTernary :: Parser String
parseTernary = do
  symbol "("
  b <- parseBoolExp
  symbol ")"
  symbol "?"
  x<- parseProgram
  symbol " : "
  y<- parseProgram
  return $ "(" ++ b ++ ")" ++ "?" ++ x ++ " : " ++ y

--Arrays
parseConcArray :: Parser String
parseConcArray = do a <- parseArray
                    symbol " <-> "
                    b <- parseArray
                    return ( a ++ b)
                <|>
                    parseArray 
  
  
  
parseArrayItems :: Parser String
parseArrayItems = do  a <- parseAexp
                      symbol ","
                      b <- parseArrayItems
                      return (a ++ "," ++ b)
                 <|> parseAexp
  
parseArray :: Parser String
parseArray = do symbol "{"
                a <- parseArrayItems
                symbol "}"
                return ("{" ++ a ++ "}")
            <|> identifier
                   
                    
concArray :: Parser [Int]
concArray = do a <- array
               symbol " <-> "
               b <- concArray
               return (a ++ b)
            <|> array
              
arrayItems :: Parser [Int]
arrayItems = do a <- integer
                symbol ","
                as <- arrayItems
                return ([a] ++ as)
            <|>
               do a <- integer
                  return [a]
                                
                   
  
array :: Parser [Int]
array =  do symbol "{"
            a <- arrayItems
            symbol "}"
            return a
          <|>
            do i <- identifier
               readArray i  



---evaluation function
eval :: String -> Env
eval c = case parse prog [] c of
  [] -> error "Invalid Input"
  [(e, _, [])] -> e
  [(e, _, out)] -> error $ "Invalid input: unused " ++ out ++ "'"

--menu

  
logo:: IO String
logo = do
  putStrLn "__________________________________________"
  putStrLn " ____________   ______          _       "
  putStrLn "|   _________| ||     \\\\       | |     "
  putStrLn "|  |_________  ||      \\\\      | |     "
  putStrLn "|   _________| ||       \\\\    / _\\\\   "
  putStrLn "|  |           ||       //   / /  \\\\   "
  putStrLn "|  |           ||      //   / /    \\\\  "
  putStrLn "|__|           ||_____//   /_/      \\\\ "
  putStrLn "                                        "
  putStrLn "        Francesco Didio Interpreter     "
  putStrLn "__________________________________________"

  start
  
start :: IO String
start = do
  putStr "FDInt> "
  hFlush stdout
  input <- getLine

  if input == "esci"
    then return "Arrivederci!"
    else do
      print (eval input)
      start
