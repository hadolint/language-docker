#!/usr/bin/env stack --silent runghc --package language-docker --package ShellCheck-0.4.4 ./dockerfile.hs
-- https://github.com/mhart/alpine-node
{-# LANGUAGE QuasiQuotes #-}

import Language.Docker

main =
    putStr $
    toDockerfileStr $
    [edockerfile|
    # https://github.com/mhart/alpine-node
    FROM mhart/alpine-node:5.5.0

    ENV DIR=/opt/este PORT=8000 \
        # This is a docker comment

        NODE_ENV=production

    RUN apk add --update python python-dev build-base git libpng automake gettext libpng-dev autoconf make zlib-dev nasm

    COPY package.json ${DIR}/

    RUN cd ${DIR} && npm install

    RUN cd ${DIR} && npm install stylus && npm install eslint-plugin-jsx-a11y

    COPY . $DIR

    WORKDIR $DIR

    RUN NODE_ENV=production SERVER_URL="https://beijaflor.io" npm run build -- -p

    EXPOSE $PORT

    ENTRYPOINT ["npm"]

    CMD ["start"]
|]
