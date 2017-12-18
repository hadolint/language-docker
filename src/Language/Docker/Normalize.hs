module Language.Docker.Normalize
    ( normalizeEscapedLines
    ) where

import Data.List (dropWhileEnd, mapAccumL)
import Data.Maybe (catMaybes)

data NormalizedLine
    = Continue
    | Joined !String
             !Int

trimLines :: [String] -> [String]
trimLines = map strip
  where
    strip = lstrip . rstrip
    lstrip = dropWhile (`elem` " \t")
    rstrip = reverse . lstrip . reverse

-- Finds all lines ending with \ and joins them with the next line using
-- a single space. If the next line is a comment, then the comment line is
-- deleted. It finally adds the same amount of new lines for each of the
-- lines it joined, in order to preser the line count in the document.
normalize :: [String] -> [String]
normalize allLines =
    let (lastState, res) = mapAccumL transform Continue allLines
    in case lastState of
           Continue -> catMaybes res
           Joined l times -> catMaybes res ++ [l ++ padNewlines times]
  where
    transform (Joined prev times) ('#':_) = (Joined prev (times + 1), Nothing)
    transform (Joined prev times) l =
        if endsWithEscape l
            then (Joined (prev ++ ' ' : normalizeLast l) (times + 1), Nothing)
            else (Continue, Just (prev ++ ' ' : l ++ padNewlines times))
    transform Continue l =
        if endsWithEscape l
            then (Joined (normalizeLast l) 1, Nothing)
            else (Continue, Just l)
    --
    endsWithEscape "" = False
    endsWithEscape s = last s == '\\'
    --
    normalizeLast = dropWhileEnd (== '\\')
    --
    padNewlines times = replicate times '\n'

-- | Remove new line escapes and join escaped lines together on one line
--   to simplify parsing later on. Escapes are replaced with line breaks
--   to not alter the line numbers.
normalizeEscapedLines :: String -> String
normalizeEscapedLines = unlines . normalize . trimLines . lines
