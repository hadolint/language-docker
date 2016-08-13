module Language.Dockerfile.ExamplesSpec where

import           Control.Monad
import           Data.Monoid
import           System.Directory
import           System.FilePath
import           System.FilePath.Glob
import           System.Process
import           Test.Hspec

stackRunGhc e = callProcess "stack" ["runghc", e]

spec :: Spec
spec = do
    cwd <- runIO getCurrentDirectory
    exampleSources <- runIO $ glob "./examples/*.hs"
    forM_ exampleSources $ \exampleSource -> do
        let exampleSource' = makeRelative cwd exampleSource
        describe exampleSource $ it ("stack runghc " <> exampleSource') $
            stackRunGhc exampleSource
