FROM ubuntu:18.04

RUN apt-get update && apt-get install --yes python python3 nodejs

ADD dist/quine-checker /usr/local/bin
RUN quine-checker
