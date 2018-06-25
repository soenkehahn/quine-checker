FROM ubuntu:18.04

RUN apt-get update && apt-get install --yes \
  ghc \
  nodejs \
  python \
  python3

ADD dist/quine-checker /usr/local/bin
RUN quine-checker
