{-# LANGUAGE FlexibleContexts #-}

module Language.Docker.Normalize
    ( normalizeEscapedLines
    ) where

import Data.List (intercalate)
import Data.List.Split (splitOn)

data NormalizedLine
    = Simple String
    | Join String
    deriving (Show)

trimLines :: [String] -> [String]
trimLines = map strip
  where
    strip = lstrip . rstrip
    lstrip = dropWhile (`elem` " \t")
    rstrip = reverse . lstrip . reverse

toNormalized :: [String] -> [NormalizedLine]
toNormalized allLines = reverse (go allLines [])
  where
    endsWithEscape "" = False
    endsWithEscape s = (head . reverse $ s) == '\\'
    startsWithComment ('#':_) = True
    startsWithComment _ = False
    go [] acc = acc
    go (l:[]) acc = (Simple l) : acc
    go (l1:l2:rest) acc =
        if not (endsWithEscape l1)
            then go (l2 : rest) (Simple l1 : acc)
            else if startsWithComment l2
                     then go rest (Join (normalizeLast l1) : acc)
                     else go (l2 : rest) (Join (normalizeLast l1) : acc)

normalizeLast :: String -> String
normalizeLast = reverse . go ""
  where
    go acc "" = acc
    go acc ('\\':rest) = go (' ' : acc) rest
    go acc (a:rest) = go (a : acc) rest

fromNormalized :: [NormalizedLine] -> [String]
fromNormalized norm = reverse (go norm [])
  where
    go [] acc = acc
    go (Join l1:Simple l2:rest) acc = go rest ((l1 ++ l2) : acc)
    go (Join l1:Join l2:rest) acc = go (Join (l1 ++ l2) : rest) acc
    go (Join l:[]) acc = l : acc
    go (Simple l:rest) acc = go rest (l : acc)

-- | Remove new line escapes and join escaped lines together on one line
--   to simplify parsing later on. Escapes are replaced with line breaks
--   to not alter the line numbers.
normalizeEscapedLines :: String -> String
normalizeEscapedLines = unlines . fromNormalized . toNormalized . trimLines . lines
