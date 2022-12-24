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
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import Control.Monad (foldM, join, liftM2)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List ((\\), foldl', sort, sortBy)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isNothing, maybe)
import GHC.Generics (Generic)
import System.CPUTime (getCPUTime)
import System.Directory (doesFileExist)
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data Edit
  = Swap Int Int
  | Indent Int Int
  | Move Int
  | Insert Int
  | Delete Int
  deriving (Eq, Ord, Show, Generic)

instance NFData Edit

maxTime :: Integer
maxTime = 59 * 10 ^ 12

main :: IO ()
main = do
  argv <- getArgs
  case (argv, readMaybe $ head argv) of
    ([], _) -> grok
    (_:ans:solns, Just max) -> do
      exists <- and <$> mapM doesFileExist (ans : solns)
      if exists
        then do
          mbEdits <- parsonsDP max ans solns
          case mbEdits of
            Nothing -> do
              print 0
              putStrLn "fail"
            Just (ansLines, solnLines, edits) -> do
              print $ max - length edits
              print edits
              putStr (explain ansLines solnLines edits)
        else exitFailure
    _ -> exitFailure

grok :: IO ()
grok = do
  let maxEdits = 10
  let ans = "answer.txt"
  let solns = ["soln1.txt"]
  let maxTime = 60 * 10 ^ 6
  let solver = parsonsDP
  secret <- getEnv "CHECKER_SECRET"
  mbEdits <- join <$> timeout maxTime (solver maxEdits ans solns)
  case mbEdits of
    Nothing ->
      putStrLn $
      "{ \"secret\": " ++
      show secret ++ ", \"score\": 0, \"output_msg\": \"too many edits\" }"
    Just (ansLines, solnLines, edits) ->
      let score =
            max 0 $
            fromIntegral (maxEdits - length edits) `div`
            fromIntegral (length edits)
       in putStrLn $
          "{ \"secret\": " ++
          show secret ++
          ", \"score\": " ++
          show score ++
          ", \"output_msg\": " ++
          show (explain ansLines solnLines edits) ++ " }"

parsons ::
     (Int -> [Int] -> [Maybe (Int, Int)] -> Maybe [Edit])
  -> Int
  -> FilePath
  -> [FilePath]
  -> IO (Maybe ([String], [String], [Edit]))
parsons solver maxEdits ans solns = do
  (ans':solns') <- map lines <$> mapM readFile (ans : solns)
  let preprocessed = concatMap (preprocess ans') solns'
  t0 <- getCPUTime
  let go curr (remaining, (solnLines, soln, ans)) = do
        t <- getCPUTime
        let remTime = (maxTime - (t - t0)) `div` (remaining * 10 ^ 6)
        next <-
          join <$>
          timeout
            (fromInteger $ min maxTime remTime)
            (forceEval $ solver (maybe maxEdits (length . thd) curr) soln ans)
        return $ combine ((ans', solnLines, ) <$> next) curr
  foldM go Nothing $ reverse $ zip [1 ..] preprocessed
  where
    combine x@(Just (_, _, x')) y@(Just (_, _, y'))
      | length x' < length y' = x
      | otherwise = y
    combine x y = x <|> y

parsonsBF ::
     Int -> String -> [String] -> IO (Maybe ([String], [String], [Edit]))
parsonsBF = parsons solveBF

parsonsDP ::
     Int -> String -> [String] -> IO (Maybe ([String], [String], [Edit]))
parsonsDP = parsons solveDP

explain :: [String] -> [String] -> [Edit] -> String
explain _ _ [] = "no edits"
explain ans soln edits = unlines $ map (("-\t" ++) . describe) edits
  where
    describe (Swap a b) =
      "swap " ++ trim (soln !! a) ++ " and " ++ trim (soln !! b)
    describe (Indent a b) =
      "indent " ++
      trim (soln !! a) ++
      if a == b
        then ""
        else " through " ++ trim (soln !! b)
    describe (Move a) = "move " ++ trim (soln !! a) ++ " into place"
    describe (Insert a) = "insert " ++ trim (soln !! a)
    describe (Delete a) = "delete " ++ trim (ans !! a)
    trim s
      | len < 20 = "'" ++ s ++ "'"
      | otherwise = "'" ++ take 17 s ++ "...'"
      where
        len = length s

preprocess :: [String] -> [String] -> [([String], [Int], [Maybe (Int, Int)])]
preprocess ans soln =
  let (ans', ansIndents) = indentations ans
      (soln', solnIndents) = indentations soln
      possibleIndexes =
        zipWith (\i x -> fix $ map (, i) x) ansIndents $
        map (lookupAll $ zip soln' [0 ..]) ans'
   in map (soln', solnIndents, ) $
      filter (((==) =<< removeDuplicates) . sort . catMaybes) $
      listProduct possibleIndexes
  where
    fix [] = [Nothing]
    fix xs = map Just xs
    indentations strs
      | scale == 0 = (strs', indents)
      | otherwise = (strs', map (`div` scale) indents)
      where
        (indents, strs') =
          first (map length) $ unzip $ map (span (`elem` " \t")) strs
        scale = foldr gcd 0 indents

clean :: [Int] -> [Maybe (Int, Int)] -> ([(Int, Int)], [Int], [Int])
clean soln ans =
  let (valid, invalid) =
        partitionEithers $
        map (uncurry ((`maybe` Left) . Right)) $ zip [0 ..] ans
      missing = filter (isNothing . (`lookup` valid)) [0 .. length soln - 1]
   in (valid, invalid, missing)

solveBF :: Int -> [Int] -> [Maybe (Int, Int)] -> Maybe [Edit]
solveBF maxEdits soln ans =
  (basic ++) . reverse <$>
  go (maxEdits - length basic) [(valid ++ map (, 0) missing, [])]
  where
    (valid, invalid, missing) = clean soln ans
    basic = map Delete invalid ++ map Insert missing
    go remaining states
      | remaining <= 0 = Nothing
      | otherwise =
        case zip [0 ..] soln `lookup` states of
          Just edits -> Just edits
          Nothing ->
            go (remaining - 1) $
            removeDuplicates $
            sortBy (compare `on` fst) $
            concatMap
              (\(st, es) -> map (second (: es)) $ bfEdits soln st)
              states

bfEdits :: [Int] -> [(Int, Int)] -> [([(Int, Int)], Edit)]
bfEdits soln state = swaps ++ indents ++ moves
  where
    len = length state
    swaps =
      [ ( map snd start ++
          [(l1, i0)] ++ map snd mid ++ [(l0, i1)] ++ map snd end
        , Swap ix0 ix1)
      | ix0 <- [0 .. len - 2]
      , let (start, (i0s, (l0, i0)):rest) = splitAt ix0 $ zip soln state
      , i0s == i0
      , ix1 <- [ix0 + 1 .. len - 1]
      , let (mid, (i1s, (l1, i1)):end) = splitAt (ix1 - ix0 - 1) rest
      , i1s == i1
      , l0 > l1
      , l0 == ix1
      , l1 == ix0
      ]
    indents =
      [ ( map snd start ++
          map (second (max 0 . (+ diff))) ((l, i) : map snd toIndent) ++
          map snd end
        , Indent ix (last $ ix : map (fst . snd) toIndent))
      | ix <- [0 .. len - 1]
      , let (start, (is, (l, i)):rest) = splitAt ix $ zip soln state
      , l == ix
      , is /= i
      , let diff = is - i
      , let match (x, (_, y)) = x - y == diff
      , ix == 0 || not (match $ last start)
      , let (toIndent, end) = span match rest
      ]
    moves =
      [ ( insertAt l (l, fromMaybe i (l `lookup` zip [0 ..] soln)) $
          start ++ end
        , Move ix)
      | ix <- [0 .. len - 2]
      , let (start, (l, i):end) = splitAt ix state
      , ix /= l
      ]

data DPEdit
  = Keep Int
  | Indent' [Int]
  | Move' Int
  deriving (Eq, Ord, Show)

solveDP :: Int -> [Int] -> [Maybe (Int, Int)] -> Maybe [Edit]
solveDP _ soln ans =
  let (valid, invalid, missing) = clean soln ans
      soln' = zip [0 ..] soln
      ans' = map (\(l, i) -> (l, fromJust (lookup l soln') - i)) valid
      edits = snd $ fst $ foldl' go ((0, []), []) ans'
   in Just $
      dpEdits ([0 .. length soln - 1] \\ missing) (reverse edits) ++
      map Delete invalid ++ map Insert missing
  where
    go (ans, memo) (line, indent) =
      let edit
            | indent == 0 = (1, [Keep line])
            | otherwise = (0, [Move' line])
          ans' = maximum $ reverse $ edit : map (next line indent) memo
       in ((max ans ans'), (line, indent, ans') : memo)
    next line indent (line', _, (score, edits))
      | line > line' && indent == 0 = (score + 1, Keep line : edits)
    next line indent (line', indent', (score, edits))
      | line > line' && indent == indent' =
        case edits of
          (Move' l):edits' -> (score + 1, Indent' [line, l] : edits')
          (Indent' ls):edits' -> (score + 1, Indent' (line : ls) : edits')
          _ -> (score, Move' line : edits)
    next line _ (_, _, (score, edits)) = (score, Move' line : edits)

dpEdits :: [Int] -> [DPEdit] -> [Edit]
dpEdits soln = go []
  where
    go seen [] = map Move (soln \\ seen)
    go seen (Keep line:rest) = go (line : seen) rest
    go seen (Indent' lines:rest) =
      Indent (last lines) (head lines) : go (lines ++ seen) rest
    go seen (Move' line:rest) = go seen rest

-- * Helpers
removeDuplicates :: (Eq a) => [(a, b)] -> [(a, b)]
removeDuplicates ((x, _):xs@((x', _):_))
  | x == x' = removeDuplicates xs
removeDuplicates (x:xs) = x : removeDuplicates xs
removeDuplicates [] = []

insertAt :: Int -> a -> [a] -> [a]
insertAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 xs = a : xs
    go n (x:xs) = x : go (n - 1) xs
    go _ [] = []

lookupAll :: (Eq a) => [(a, b)] -> a -> [b]
lookupAll pairs a = map snd $ filter ((a ==) . fst) pairs

listProduct :: [[a]] -> [[a]]
listProduct [] = [[]]
listProduct (x:xs) = (:) <$> x <*> listProduct xs

forceEval :: (NFData a) => a -> IO a
forceEval a = evaluate a >>= liftM2 seq rnf return

thd :: (a, b, c) -> c
thd (_, _, x) = x
