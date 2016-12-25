module Brainhask where

import Parser
import Data.Char (ord, chr)
import Control.Monad.Fail
import Prelude hiding (fail)

data BFState = BFState [Int] [Int]

data BFStruc = Block [BFStruc] | Atom BFCommand
data BFCommand = R | L | Plus | Minus | Out | In

{- CFG
program -> *struc
struc -> block | atom | commentChar
block -> [ *struc ]
atom -> > | < | + | - | . | ,
-}

bf :: String -> IO ()
bf prog = case getResult $ tryParse bfParse prog of 
               Nothing -> error "Invalid program."
               Just ast -> bfExec ast >> return ()

bfParse :: Parser String [BFStruc]
bfParse = fmap filterJust $ kleeneStar bfParseStruc where
    filterJust = foldr (\e acc-> case e of Just x -> x:acc; Nothing -> acc) []

bfParseStruc :: Parser String (Maybe BFStruc) --Maybe is for comments
bfParseStruc = fmap (either (Just . either Block Atom) id) (bfParseBlock <|> bfParseAtom <|> bfParseComment)

bfParseComment :: Parser String (Maybe BFStruc)
bfParseComment = do 
    x <- parseAnyChar
    if x `elem` "><+-.,[]" then fail "" else return Nothing

bfParseBlock :: Parser String [BFStruc]
bfParseBlock = do
    parseChar '['
    inside <- bfParse --Already a shortcut to parse *struc
    parseChar ']'
    return inside
    
bfParseAtom :: Parser String BFCommand
bfParseAtom = parseAnyChar >>= \x -> case x of
    '>' -> return R
    '<' -> return L
    '+' -> return Plus
    '-' -> return Minus
    '.' -> return Out
    ',' -> return In
    otherwise -> fail ""

bfExec :: [BFStruc] -> IO BFState
bfExec = foldl (\acc e -> acc >>= bfExecStruc e) (return $ BFState [] (repeat 0))

bfExecStruc :: BFStruc -> BFState -> IO BFState
bfExecStruc (Atom a) (BFState l (r:rs)) = case a of
                            R -> return $ BFState (r:l) rs
                            L -> case l of 
                                      [] -> return $ BFState l (r:rs)
                                      (x:xs) -> return $ BFState xs (x:r:rs)
                            Plus -> return $ BFState l (succ r:rs)
                            Minus -> return $ BFState l (pred r:rs)
                            Out -> putChar (chr r) >> (return $ BFState l (r:rs))
                            In -> getChar >>= \x -> return $ BFState l (ord x:rs)
bfExecStruc (Block b) (s@(BFState l (r:rs))) = case r of 
                            0 -> return s
                            otherwise -> foldl (\acc e -> acc >>= bfExecStruc e) (return s) b >>= \(x@(BFState xl (xr:xrs))) -> case xr of
                                              0 -> return x
                                              otherwise -> bfExecStruc (Block b) x
