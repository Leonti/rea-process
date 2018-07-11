#!/usr/bin/env bash
set -e

version=$(date +"%y.%m.%d.%H.%M")

docker login -u="$DOCKER_USERNAME" -p="$DOCKER_PASSWORD"
docker run -v "$(pwd):/src" -v /tmp/stack:/root/.stack -w /src --rm haskell:8.2.2 stack build --allow-different-user -- .

docker build -t leonti/rea-process:$version .
docker push leonti/rea-process:$version

echo $version" is built"
