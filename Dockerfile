FROM alpine:3.6

RUN apk add --no-cache --update curl zip bash

WORKDIR /root

COPY .stack-work/install/x86_64-linux/lts-11.0/8.2.2/bin/rea-process rea-process
COPY stops.csv stops.csv
COPY stores.csv stores.csv

CMD ./rea-process
