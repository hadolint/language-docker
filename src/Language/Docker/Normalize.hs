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
-- lines it joined, in order to preserve the line count in the document.
normalize :: [String] -> [String]
normalize allLines =
    let (lastState, res) -- mapAccumL is the idea of a for loop with a variable holding
                         -- some state and another variable where we append the final result
                         -- of the looping operation. For each line in lines, apply the transform
                         -- function. This function always returns a new state, and another element
                         -- to append to the final result. The ending result of mapAccumL is the final
                         -- state variale and the resulting list of values. We initialize the loop with
                         -- the 'Continue' state, which means "no special action to do next"
         = mapAccumL transform Continue allLines
    in case lastState of
           Continue -- The last line of the document is a normal line, cleanup and return
            -> catMaybes res
           Joined l times -- The last line contains a \, so we need to add the buffered
                          -- line back to the result, pad with newlines and cleanup
            -> catMaybes res ++ [l ++ padNewlines times]
  where
    normalizeLast = dropWhileEnd (== '\\')
    -- | Checks the result of the previous operation in the loop (first argument)
    --
    -- If the previous result is a 'Joined' operation, then we merge the previous
    -- and the current line in a single line and return it.
    --
    -- If the current line ends with a \, then we produce a 'Joined' state as result
    -- of this looping operation.
    --
    -- If the previous 2 conditions are true at the same time, then we produce a new
    -- 'Joined' state holding the concatenation of the Joined buffer and the previous line
    -- and we return 'Nothing' as an indication that this line does not form part of the
    -- final result.
    transform :: NormalizedLine -> String -> (NormalizedLine, Maybe String)
    -- If we are buffering lines, and the next line is a comment,
    -- we simply ignore the comment and remember to add a newline
    transform (Joined prev times) ('#':_) = (Joined prev (times + 1), Nothing)
    -- If we are buffering lines, then we check whether the current line end with \,
    -- if it does, then we merged it into the buffered state, otherwise we just yield
    -- the concatanation of the buffer and the current line as result, after padding with
    -- newlines
    transform (Joined prev times) l =
        if endsWithEscape l
            then (Joined (prev ++ ' ' : normalizeLast l) (times + 1), Nothing)
            else (Continue, Just (prev ++ ' ' : l ++ padNewlines times))
    -- When not buffering lines, then we just check if we need to start doing it by checking
    -- whether or not the current line ends with \. If it does not, then we just yield the
    -- current line as part of the result
    transform Continue l =
        if endsWithEscape l
            then (Joined (normalizeLast l) 1, Nothing)
            else (Continue, Just l)
    --
    endsWithEscape "" = False
    endsWithEscape s = last s == '\\'
    --
    padNewlines times = replicate times '\n'

-- | Remove new line escapes and join escaped lines together on one line
--   to simplify parsing later on. Escapes are replaced with line breaks
--   to not alter the line numbers.
normalizeEscapedLines :: String -> String
normalizeEscapedLines = unlines . normalize . trimLines . lines
