import qualified Data.Text.Lazy.IO as L
import Language.Docker

main = do
    Right d <- parseFile "./Dockerfile"
    L.putStr (prettyPrint d)
