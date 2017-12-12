import Language.Docker
main = do
    c <- readFile "./Dockerfile"
    print (parseString c)
