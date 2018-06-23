FROM ubuntu:18.04

RUN apt-get update && apt-get install --yes python python3 nodejs curl

RUN curl -sSL https://get.haskellstack.org/ | sh
# installing ghc and prewarming cache
ADD tests/haskell.hs /root
RUN stack --terminal /root/haskell.hs

ADD dist/quine-checker /usr/local/bin
RUN quine-checker
