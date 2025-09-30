
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-} -- this silences an uninteresting warning

module Set13a where

import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.List
import qualified Data.Map as Map

main = printExs

add :: Int -> State Int ()
add i = do 
  old <- get
  put (old+i)

example :: Int -> State Int Int
example n = do 
  x1dummy <- add n           
  value <- get
  let y1dummy = 3
  let factor = 2
  x2dummy <- put (factor*value)    
  return value    

printExs :: IO ()
printExs = do 
  let st = example 4 
  let ms0 = [] :: [State Int Int]
  let ms1 = [example 4] 
  let ms2 = ms1 ++ [example 1]
  let ms3 = ms2 ++ [example 8]
  putStrLn "\nrunState (example 4) 7"
  putStrLn $ show $ runState st 7
  putStrLn "\nrunState (sequence []) 7"
  putStrLn $ show $ runState (sequence ms0) 7
  putStrLn "\nrunState (sequence [example 4]) 7"
  putStrLn $ show $ runState (sequence ms1) 7
  putStrLn "\nrunState (sequence [example 4, example 1]) 7"
  putStrLn $ show $ runState (sequence ms2) 7
  putStrLn "\nrunState (sequence [example 4, example 1, example 8]) 7"
  putStrLn $ show $ runState (sequence ms3) 7
  putStrLn "\nrunState (sequence [example 8, example 1, example 4]) 7"
  putStrLn $ show $ runState (sequence (reverse ms3)) 7