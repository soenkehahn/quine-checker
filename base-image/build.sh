#!/usr/bin/env bash

set -eux

stack test
stack install --local-bin-path base-image/dist

docker build --tag soenkehahn/rc-quines-candidate ./base-image
./base-image/test-base-image.hs
docker build --tag soenkehahn/rc-quines ./base-image
