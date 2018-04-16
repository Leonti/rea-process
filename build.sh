#!/usr/bin/env bash
set -e

version=$(date +"%y.%m.%d.%H.%M")

docker login -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD"
sudo docker run -v "$(pwd):/src" -v /tmp/stack:/root/.stack --rm leonti/haskell-static-build:18.04.04.03.12

sudo docker build -t leonti/rea-process:$version .
sudo docker push leonti/rea-process:$version

echo $version" is built"
