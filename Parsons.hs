{-|
Module      : Parsons
Description : Parsons Problem Marker in Haskell 
Copyright   : (c) James Barnes (2022)
Maintainer  : jamesbarns2505@gmail.com
Stability   : experimental
-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.List
import Data.Maybe
import Data.Either
import Data.Function
import Text.Read
import Control.Monad
import Control.Arrow
import Control.Applicative
import Control.DeepSeq
import Control.Exception
import System.Environment
import System.CPUTime
import System.Timeout
import System.Exit
import System.Directory
import GHC.Generics

data Edit = Swap Int Int
          | Indent Int Int
          | Cycle Int Int
          | Insert Int
          | Delete Int
  deriving (Eq, Ord, Show, Generic)

instance NFData Edit


maxTime :: Integer
maxTime = 1 * 10 ^ 12


main :: IO ()
main = do
    argv <- getArgs
    case (argv, readMaybe $ head argv) of
        (_:ans:solns, Just max) -> do
            exists <- mapM doesFileExist (ans:solns)
            if and exists
            then do
                mbEdits <- parsons max ans solns
                case mbEdits of
                    Nothing -> do
                        print 0
                        putStrLn "fail"
                    Just edits -> do 
                        print $ max - length edits
                        print edits
            else exitFailure
        _ -> exitFailure


parsons :: Int -> String -> [String] -> IO (Maybe [Edit])
parsons max ans solns = do
    (ans':solns') <- map lines <$> mapM readFile (ans:solns)
    let preprocessed = concatMap (preprocess ans') solns'
    t0 <- getCPUTime
    foldM (go t0) Nothing $ reverse $ zip [1..] preprocessed
  where
    go t0 curr (remaining, (soln, ans)) = do
        t <- getCPUTime
        let remTime = (maxTime - (t - t0)) `div` (remaining * 10 ^ 6)
        next <- timeout (fromInteger $ min maxTime remTime) 
            $ forceEval $ solve (maybe max length curr) soln ans
        return $ join next <|> curr


preprocess :: [String] -> [String] -> [([Int], [Maybe (Int, Int)])]
preprocess ans soln = 
    let (ans', ansIndents) = indentations ans
        (soln', solnIndents) = indentations soln
        possibleIndexes = zipWith (\i x -> fixup $ map (,i) x) ansIndents
                        $ map (lookupAll $ zip soln' [0..]) ans'
        fixup [] = [Nothing]
        fixup xs = map Just xs
    in map (solnIndents,) 
        $ filter (((==) =<< removeDuplicates) . sort . catMaybes)
        $ listProduct possibleIndexes


indentations :: [String] -> ([String], [Int])
indentations strs = 
    let (indents, strs') = first (map length) $ unzip $ map (span (' ' ==)) strs
        scale = foldr gcd 0 indents
    in (strs', if scale == 0 then indents else map (`div` scale) indents)


solve :: Int -> [Int] -> [Maybe (Int, Int)] -> Maybe [Edit]
solve maxEdits soln ans 
    = (basic ++) . reverse 
   <$> go (maxEdits - length basic) [(valid ++ map (,0) missing, [])]
  where
    (valid, invalid) = partitionEithers 
                     $ map (uncurry ((`maybe` Left) . Right))
                     $ zip [0..] ans 
    missing = filter (isNothing . (`lookup` valid)) [0..length soln - 1]
    basic = map Delete invalid ++ map Insert missing

    go remaining states
      | remaining <= 0 = Nothing
      | otherwise = 
        case zip [0..] soln `lookup` states of
            Just edits -> 
                Just edits
            Nothing -> 
                go (remaining - 1) 
                    $ removeDuplicates 
                    $ sortBy (compare `on` fst) 
                    $ concatMap (\(st, es) -> map (second (:es)) $ edits soln st) 
                        states


edits :: [Int] -> [(Int, Int)] -> [([(Int, Int)], Edit)]
edits soln state
    = swaps ++ indents ++ cycles
  where
    len = length state
    swaps = 
        [ ( map snd start
            ++ [(l1, i0)]
            ++ map snd mid
            ++ [(l0, i1)] 
            ++ map snd end 
          , Swap ix0 ix1
          )
        | ix0 <- [0..len-2]
        , let (start, (i0s, (l0, i0)):rest) = splitAt ix0 $ zip soln state
        , i0s == i0
        , ix1 <- [ix0+1..len-1]
        , let (mid, (i1s, (l1, i1)):end) = splitAt (ix1 - ix0 - 1) rest
        , i1s == i1
        , l0 > l1
        , l0 == ix1
        , l1 == ix0
        ]
    indents = 
        [ ( map snd start 
            ++ map (second (max 0 . (+ diff))) ((l, i):map snd toIndent) 
            ++ map snd end
          , Indent ix (1 + length toIndent)
          )
        | ix <- [0..len-1]
        , let (start, (is, (l, i)):rest) = splitAt ix $ zip soln state
        , l == ix
        , is /= i
        , let diff = is - i
        , let match (x, (_, y)) = signum (x - y) == signum diff
        , ix == 0 || not (match $ last start)
        , let (toIndent, end) = span match rest
        ]
    cycles =
        [ ( insertAt l (l, fromMaybe i (l `lookup` zip [0..] soln)) 
            $ start ++ end
          , Cycle ix l
          )
        | ix <- [0..len-2]
        , let (start, (l, i):end) = splitAt ix state
        , ix /= l
        ]


removeDuplicates :: Ord a => [(a, b)] -> [(a, b)]
removeDuplicates ((x,_):xs@((x',_):_)) 
  | x == x' = removeDuplicates xs
removeDuplicates (x:xs)   = x : removeDuplicates xs
removeDuplicates []       = []


insertAt :: Int -> a -> [a] -> [a]
insertAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 xs     = a : xs
    go n (x:xs) = x : go (n - 1) xs
    go _ []     = []


lookupAll :: (Eq a) => [(a, b)] -> a -> [b]
lookupAll pairs a = map snd $ filter ((a ==) . fst) pairs


listProduct :: [[a]] -> [[a]]
listProduct []     = [[]]
listProduct (x:xs) = (:) <$> x <*> listProduct xs

forceEval :: (NFData a) => a -> IO a
forceEval a = evaluate a >>= liftM2 seq rnf return
