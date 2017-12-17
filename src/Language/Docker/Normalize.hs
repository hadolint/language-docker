{-# LANGUAGE FlexibleContexts #-}

module Language.Docker.Normalize
    ( normalizeEscapedLines
    ) where

import Data.List (dropWhileEnd, mapAccumL)
import Data.Maybe (catMaybes)

data NormalizedLine
    = Continue
    | Joined String

trimLines :: [String] -> [String]
trimLines = map strip
  where
    strip = lstrip . rstrip
    lstrip = dropWhile (`elem` " \t")
    rstrip = reverse . lstrip . reverse

normalize :: [String] -> [String]
normalize allLines =
    let (_, res) = mapAccumL transform Continue allLines
    in catMaybes res
  where
    transform (Joined prev) ('#':_) = (Joined prev, Nothing)
    transform (Joined prev) l =
        if endsWithEscape l
            then (Joined $ prev ++ normalizeLast l, Nothing)
            else (Continue, Just (prev ++ ' ' : l))
    transform Continue l =
        if endsWithEscape l
            then (Joined (normalizeLast l), Nothing)
            else (Continue, Just l)
    --
    endsWithEscape "" = False
    endsWithEscape s = last s == '\\'
    --
    normalizeLast = dropWhileEnd (== '\\')

-- | Remove new line escapes and join escaped lines together on one line
--   to simplify parsing later on. Escapes are replaced with line breaks
--   to not alter the line numbers.
normalizeEscapedLines :: String -> String
normalizeEscapedLines = unlines . normalize . trimLines . lines
