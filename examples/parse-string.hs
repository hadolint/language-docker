import Language.Docker

main = do
    c <- parseFile "./Dockerfile"
    print c
