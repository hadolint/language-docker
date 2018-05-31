FROM fpco/stack-build:lts-6.9
ADD . /app/language-docker
WORKDIR /app/language-docker
RUN stack build --test --only-dependencies
CMD stack test
