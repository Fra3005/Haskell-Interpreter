-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Use <$>" #-}
-- {-# HLINT ignore "Redundant return" #-}
-- module Lib(menu) where

-- import Data.Char
-- import System.IO
-- import Control.Applicative



-- data Variable = Variable {
--     name :: String,
--     vtype :: String,
--     value :: Int
-- } deriving Show



-- type Env = [Variable]


-- updateEnv :: Variable -> Parser String
-- updateEnv var = P (\env input -> case input of
--                      xs -> [((modifyEnv env var),"",xs)])

-- modifyEnv :: Env -> Variable -> Env
-- modifyEnv [] var = [var]
-- modifyEnv (x:xs) newVar = if (name x) == (name newVar) then
--                             [newVar] ++ xs
--                           else
--                             [x] ++ modifyEnv xs newVar

-- --enviroment  management
-- readVariable :: String -> Parser Int
-- readVariable name = P (\env input -> case searchVariable env name of
--     [] -> []
--     [value] -> [(env, value , input)])

-- searchVariable :: Env -> String -> [Int]
-- searchVariable [] queryname = []
-- searchVariable (x:xs) queryname = if (name x) == queryname
--                                        then [(value x)]
--                                        else
--                                             searchVariable xs queryname



-- newtype Parser a = P (Env -> String -> [(Env, a, String)])

-- parse :: Parser a -> Env -> String -> [(Env, a, String)]
-- parse (P p) env inp = p env inp

-- instance Functor Parser where
--     fmap g p = P (\env inp -> case parse p env inp of
--         [] -> []
--         [(env, v, out)] -> [(env, g v, out)])

-- instance Applicative Parser where
--     pure v = P (\env inp -> [(env,v,inp)])

--     pg <*> px = P (\env inp -> case parse pg env inp of
--         [] -> []
--         [(env,g,out)] -> parse (fmap g px) env out)

-- instance Monad Parser where
--     return v = P (\env inp -> [(env,v,inp)])

--     p >>= f = P (\env inp -> case parse p env inp of
--         [] -> []
--         [(env,v,out)] -> parse (f v) env out)

-- instance Alternative Parser where
--     empty = P (\env inp -> [])

--     p <|> q = P (\env inp -> case parse p env inp of
--         [] -> parse q env inp
--         [(env,v,out)] -> [(env,v,out)])

-- --Parser function
-- item :: Parser Char
-- item = P (\env inp -> case inp of
--     [] -> []
--     (x:xs) -> [(env, x, xs)])

-- sat :: (Char -> Bool) -> Parser Char
-- sat p = do {
--     x <- item;
--     if p x then return x else empty;
-- }

-- --Syntax

-- digit :: Parser Char
-- digit = sat isDigit

-- char :: Char -> Parser Char
-- char x = sat (== x)

-- lower :: Parser Char
-- lower = sat isLower

-- upper :: Parser Char
-- upper = sat isUpper

-- letter :: Parser Char
-- letter = sat isAlpha

-- alphaNum :: Parser Char
-- alphaNum = sat isAlphaNum

-- string :: String -> Parser String
-- string [] = return []
-- string (x:xs) = do {
--     char x;
--     string xs;
--     return (x:xs);
--     }

-- ident :: Parser String
-- ident = do {
--     removeSpaces;
--     x <- lower;
--     xs <- many alphaNum;
--     removeSpaces;
--     return (x:xs);
--     }

-- removeSpaces :: Parser ()
-- removeSpaces =
--     do {
--     many (sat isSpace);
--     return ();
--     }

-- --non funziona da rifare
-- token :: Parser a -> Parser a         -- deletes spaces
-- token p =
--     do
--         {many (sat isSpace);
--         v <- p;
--         many (sat isSpace);
--         return v;}


-- identifier :: Parser String
-- identifier = token ident

-- natural :: Parser Int
-- natural = do {
--     xs <- some digit;
--     return (read xs);

-- }

-- integer :: Parser Int
-- integer = do { char '-';
--               n <- natural;
--               return (-n);}
--           <|>
--               natural

-- space :: Parser ()
-- space = do {
--     many (sat isSpace);
--     return ();
--     }

-- symbol :: String -> Parser String
-- symbol [] = return ""
-- symbol (x:xs) = do {sat (x ==);
--                     symbol xs;
--                     return (x:xs);}

-- --Defining parser for arithmetic expression
-- aExp :: Parser Int
-- aExp = do {
--     do{
--         t <- aTerm;
--         symbol "+";
--         a <- aExp;
--         return (t + a);
--     } <|>
--     do {
--         t <- aTerm;
--         symbol "-";
--         a <- aExp;
--         return (t - a);
--     } <|>
--     do {aTerm}
-- }

-- aTerm :: Parser Int
-- aTerm = do{
--     do{
--         f <- aFactor;
--         symbol "*";
--         a <- aTerm;
--         return (f * a);
--     } <|>
--     do{
--         f <- aFactor;
--         symbol "/";
--         a <- aTerm;
--         return (f `div` a);
--     } <|>
--     do{aFactor}
-- }

-- aFactor :: Parser Int
-- aFactor = do{
--     do {
--         symbol "(";
--         a <- aExp;
--         symbol ")";
--         return a
--     } <|>
--     do{
--         i <- ident;
--         readVariable i;
--     }<|>
--     do{
--         integer;
--     }
-- }
-- --Consume aExp

-- paraExp :: Parser String
-- paraExp = do{

--     do{
--         t <- paraTerm;
--         symbol "+";
--         a <- paraExp;
--         return (t ++ "+" ++ a);
--     } <|>
--     do{
--         t <- paraTerm;
--         symbol "-";
--         a <- paraExp;
--         return (t ++ "-" ++ a)
--     } <|>
--     do {
--         paraTerm;
--     }
-- }

-- paraTerm :: Parser String
-- paraTerm = do {

--     do{
--         f <- paraFactor;
--         symbol "*";
--         t <- paraTerm;
--         return (f ++ "*" ++ t);
--     } <|>
--     do{
--         f <- paraFactor;
--         symbol "/";
--         t <- paraTerm;
--         return (f ++ "/" ++ t);
--     } <|>
--     do {
--         paraFactor;
--     }
-- }

-- paraFactor :: Parser String
-- paraFactor = do{
--     do{
--         symbol "(";
--         a <- paraExp;
--         symbol ")";
--         return ("(" ++ a ++ ")");
--     }<|>
--     do{
--         i <- ident;
--         return i;
--     }
--     <|>
--     do {
--         i <- integer;
--         return (show i);
--        }
-- }

-- --Boolean Expression
-- bExp :: Parser Bool
-- bExp = do{
--         b1 <- bTerm;
--         symbol "oo";
--         b2 <- bExp;
--         return (b1 || b2);
--     }<|>
--     do{bTerm;}

-- bTerm :: Parser Bool
-- bTerm =
--     do {
--         b1 <- bFactor;
--         symbol "aa";
--         b2 <- bTerm;
--         return (b1 && b2)
--     }<|>
--     do{bFactor;}

-- bFactor :: Parser Bool
-- bFactor = do{
--     do{
--         symbol "(";
--         b <- bExp;
--         symbol ")";
--         return b;
--     }<|>
--     do{
--         symbol "Vero";
--         return True;
--     }<|>
--     do{
--         symbol "false";
--         return False;
--     }<|>
--     do{
--         symbol "nn";
--         b <- bFactor;
--         return (not b);
--     } <|>
--     do{bCompersion;}
-- }

-- bCompersion :: Parser Bool
-- bCompersion = do{
--     do {
--         a <- aExp;
--         symbol "=";
--         b <- aExp;
--         return (a == b);
--     }<|>
--     do{
--         a <- aExp;
--         symbol "<=";
--         b <- aExp;
--         return (a <= b);
--     }<|>
--     do{
--         a <- aExp;
--         symbol ">=";
--         b <- aExp;
--         return (a >= b);
--     }<|>
--     do{
--         a <- aExp;
--         symbol ">";
--         b <- aExp;
--         return (a > b);
--     }<|>
--     do{
--         a <- aExp;
--         symbol "<";
--         b <- aExp;
--         return (a < b);
--     }
-- }

-- --Consume bExp
-- parbExp :: Parser String
-- parbExp = do{
--     do{
--         b1 <- parbTerm;
--         symbol "oo";
--         b2 <- parbExp;
--         return (b1 ++ "oo" ++ b2);
--     }<|>
--     do{parbTerm;}
-- }

-- parbTerm :: Parser String
-- parbTerm = do{
--     do {
--         b1 <- parbFactor;
--         symbol "aa";
--         b2 <- parbTerm;
--         return (b1 ++ "aa" ++ b2)
--     }<|>
--     do{parbFactor;}
-- }

-- parbFactor :: Parser String
-- parbFactor = do{
--     do{
--         symbol "(";
--         b <- parbExp;
--         symbol ")";
--         return ("(" ++ b ++ ")");
--     }<|>
--     do{
--         symbol "Vero";
--         return "Vero";
--     }<|>
--     do{
--         symbol "false";
--         return "false";
--     }<|>
--     do{
--         symbol "nn";
--         b <- parbFactor;
--         return ("nn" ++ b);
--     } <|>
--     do{parbCompersion;}
-- }

-- parbCompersion :: Parser String
-- parbCompersion = do{
--     do {
--         a <- paraExp;
--         symbol "=";
--         b <- paraExp;
--         return (a ++ "=" ++ b);
--     }<|>
--     do{
--         a <- paraExp;
--         symbol "<=";
--         b <- paraExp;
--         return (a ++ "<=" ++ b);
--     }<|>
--     do{
--         a <- paraExp;
--         symbol ">=";
--         b <- paraExp;
--         return (a ++ ">=" ++ b);
--     }<|>
--     do{
--         a <- paraExp;
--         symbol ">";
--         b <- paraExp;
--         return (a ++ ">" ++ b);
--     }<|>
--     do{
--         a <- paraExp;
--         symbol "<";
--         b <- paraExp;
--         return (a ++ "<" ++ b);
--     }
-- }

-- --Commands
-- program :: Parser String
-- program = do{
--     do {
--         command;
--         program;
--     }<|>
--     do {
--         command;
--     }
-- }

-- parProgram :: Parser String
-- parProgram = do{
--     do{
--         c <- parCommand;
--         p <- parProgram;
--         return (c ++ p)
--     }<|>
--     do{
--         c <- parCommand;
--         return c
--     }
-- }

-- command :: Parser String
-- command = do{
--     assignment
--     <|>
--     ifElse
--     <|>
--     whileST
--     <|>
--     do{
--         symbol "skip";
--         symbol ".";}
-- }

-- parCommand :: Parser String
-- parCommand = do{parAssignment <|> parIfElse <|> whileST <|> symbol "skip" <|> symbol "."}

-- executeWhile :: String -> Parser String
-- executeWhile c = P(\env input -> [(env, "", c ++ input)])

-- whileST :: Parser String
-- whileST = do{
--     w <- parWhileST;
--     executeWhile w;
--     symbol "while";
--     removeSpaces; symbol "(";
--     removeSpaces; b <- bExp;
--     removeSpaces; symbol ")";
--     removeSpaces; symbol "{";
--     if b then
--             do {
--             program;
--             symbol "}";
--             executeWhile w;
--             whileST;}
--     else
--             do{
--                 parProgram;
--                 symbol "}";
--                 return "";}
--     }<|>
--     do{
--         w <- parWhileST;
--         executeWhile w;
--         symbol "do";
--         removeSpaces; symbol "{";
--         program;
--         removeSpaces; symbol "}";
--         removeSpaces; symbol "while";
--         removeSpaces; symbol "(";
--         removeSpaces; b <- bExp;
--         removeSpaces; symbol ");";
--         if b then
--             do{
--                 executeWhile w;
--                 whileST;
--             }
--         else
--             do{
--                 return "";
--             }
--     }

-- parWhileST :: Parser String
-- parWhileST = do {symbol "while";
--                 removeSpaces; symbol "(";
--                 removeSpaces; b <- parbExp;
--                 removeSpaces; symbol ")";
--                 removeSpaces; symbol "{";
--                 removeSpaces; x <- parProgram;
--                 removeSpaces; symbol "}";
--                 return ("while(" ++ b ++ "){" ++ x ++ "}");}
--             <|>
--             do {
--                 symbol "do";
--                 removeSpaces; symbol "{";
--                 removeSpaces; p <- parProgram;
--                 removeSpaces; symbol "}";
--                 removeSpaces; symbol "while";
--                 removeSpaces; symbol "(";
--                 removeSpaces; b <- parbExp;
--                 removeSpaces; symbol ");";
--                 return ("do{" ++ p ++ "}while(" ++ b ++ ");")
--             }

-- ifElse :: Parser String
-- ifElse = do{
--     symbol "if(";
--     b <- bExp;
--     symbol ")";
--     if b then
--         do{
--             symbol "{";
--             program;
--             symbol "}else{";
--             parProgram;
--             symbol "}";
--             return ""
--         }
--     else
--         do{parProgram;
--         symbol "else{";
--         program;
--         symbol "}";
--         return "";
--         }
--     }<|>
--     do {
--         symbol "if(";
--         b <- bExp;
--         symbol ")";
--         if b then
--             do{
--                 symbol "{";
--                 program;
--                 symbol "}";
--                 return "";
--             }
--         else
--             do{parProgram;
--             return "";
--             }
--     }
-- parIfElse :: Parser String
-- parIfElse = do{
--     symbol "if";
--     removeSpaces; symbol "(";
--     removeSpaces; b <- parbExp;
--     removeSpaces; symbol ")";
--     removeSpaces; symbol "{";
--     removeSpaces; p1 <- parProgram;
--     removeSpaces; symbol "}else{";
--     removeSpaces; p2 <- parProgram;
--     removeSpaces; symbol "}";
--     return ("if(" ++ b ++ "){" ++ p1 ++ "}else{" ++ p2 ++ "}")
--     }<|>
--     do{
--         symbol "if";
--         removeSpaces; symbol "(";
--         removeSpaces; b <- parbExp;
--         removeSpaces; symbol ")";
--         removeSpaces; symbol "{";
--         removeSpaces; p <- parProgram;
--         removeSpaces; symbol "}";
--         return ("if(" ++ b ++ "){" ++ p ++ "}");
--     }

-- assignment :: Parser String
-- assignment =
--     do{
--         x <- ident;
--         symbol ":=";
--         v <- aExp;
--         symbol ".";
--         updateEnv Variable{name = x, vtype = "Integer", value = v};}



-- parAssignment :: Parser String
-- parAssignment = do {
--     x <- ident;
--     symbol ":=";
--     a <- paraExp;
--     symbol ".";
--     return (x ++ ":=" ++ a ++ ".")
--     }

-- eval :: String -> Env
-- eval c = case parse program [] c of
--              []          -> error "Invalid Input"
--              [(e, _, [])]  -> e
--              [(e, _, out)] -> error $ "Invalid input: unused '" ++ out ++ "'"

-- menu :: IO String
-- menu = do {
--     input <- getLine;

--     if (input == "exit") then return "Bye!"
--     else do {
--         print (eval input);
--         menu;
--         }
--     }