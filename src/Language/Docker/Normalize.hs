module Language.Docker.Normalize
    ( normalizeEscapedLines
    ) where

import Data.List (mapAccumL)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Builder as Builder

data NormalizedLine
    = Continue
    | Joined !Builder.Builder
             !Int

-- Finds all lines ending with \ and joins them with the next line using
-- a single space. If the next line is a comment, then the comment line is
-- deleted. It finally adds the same amount of new lines for each of the
-- lines it joined, in order to preserve the line count in the document.
normalize :: Text -> Text
normalize allLines =
    let (lastState, res) -- mapAccumL is the idea of a for loop with a variable holding
                         -- some state and another variable where we append the final result
                         -- of the looping operation. For each line in lines, apply the transform
                         -- function. This function always returns a new state, and another element
                         -- to append to the final result. The ending result of mapAccumL is the final
                         -- state variale and the resulting list of values. We initialize the loop with
                         -- the 'Continue' state, which means "no special action to do next"
         = mapAccumL transform Continue (Text.lines allLines)
    in case lastState of
           Continue -- The last line of the document is a normal line, cleanup and return
            -> Text.unlines . catMaybes $ res
           Joined l times -- The last line contains a \, so we need to add the buffered
                          -- line back to the result, pad with newlines and cleanup
            -> Text.unlines (catMaybes res <> [toText (l <> padNewlines times)])
  where
    toText = toStrict . Builder.toLazyText
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
    transform :: NormalizedLine -> Text -> (NormalizedLine, Maybe Text)
    transform (Joined prev times) rawLine
        -- If we are buffering lines and the next one is empty or it starts with a comment
        -- we simply ignore the comment and remember to add a newline
        | Text.null line || isComment line = (Joined prev (times + 1), Nothing)
        -- If we are buffering lines, then we check whether the current line end with \,
        -- if it does, then we merged it into the buffered state
        | endsWithEscape line = (Joined (prev <> normalizeLast line) (times + 1), Nothing)
        -- otherwise we just yield
        -- the concatanation of the buffer and the current line as result, after padding with
        -- newlines
        | otherwise = (Continue, Just (toText (prev <> Builder.fromText line <> padNewlines times)))
      where
        line = Text.stripEnd rawLine
    -- When not buffering lines, then we just check if we need to start doing it by checking
    -- whether or not the current line ends with \. If it does not, then we just yield the
    -- current line as part of the result
    transform Continue rawLine
        | isComment line = (Continue, Just line)
        | endsWithEscape line = (Joined (normalizeLast line) 1, Nothing)
        | otherwise = (Continue, Just line)
      where
        line = Text.stripEnd rawLine
    --
    endsWithEscape t
        | Text.null t = False
        | otherwise = Text.last t == '\\'
    --
    padNewlines times = Builder.fromText (Text.replicate times (Text.singleton '\n'))
    --
    normalizeLast = Builder.fromText . Text.dropWhileEnd (== '\\')
    --
    isComment line =
        case (Text.uncons . Text.stripStart) line of
            Just ('#', _) -> True
            _ -> False

-- | Remove new line escapes and join escaped lines together on one line
--   to simplify parsing later on. Escapes are replaced with line breaks
--   to not alter the line numbers.
normalizeEscapedLines :: Text -> Text
normalizeEscapedLines = normalize

{-# INLINE normalizeEscapedLines #-}
