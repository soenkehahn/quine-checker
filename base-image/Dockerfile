FROM ubuntu:18.04

ENV DEBIAN_FRONTEND='noninteractive'

RUN apt-get update && apt-get install --yes \
  ghc \
  nodejs \
  python \
  python3 \
  ruby \
  r-base-core

ADD dist/quine-checker /usr/local/bin
RUN quine-checker
