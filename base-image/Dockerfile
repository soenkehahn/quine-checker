FROM ubuntu:18.04

ADD dist/quine-checker /usr/local/bin
RUN quine-checker

RUN apt-get update && apt-get install --yes python python3
