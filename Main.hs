module Main where

import Brainhask
import System.Environment

main = do 
    args <- getArgs
    contents <- getContents
    if "-i" `elem` args
        then bfInterpret contents
        else case bf contents of
            Nothing -> error "Invalid bf program."
            Just prog -> print prog
