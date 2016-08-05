FROM fpco/stack-build:lts-6.9
ADD . /app/language-dockerfile
WORKDIR /app/language-dockerfile
RUN stack build --test --only-dependencies
CMD stack test
