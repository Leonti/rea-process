FROM haskell:8.2.2
RUN apt-get update && \
    apt-get install -y ca-certificates && \
    apt-get install -y libgnutls30 && \
    apt-get install -y netbase && \
    apt-get -y autoremove && \
    apt-get -y clean && \
    rm -rf /var/lib/apt/lists/* && \
    rm -rf /tmp/* && \
    rm -rf /var/tmp/*

WORKDIR /root

COPY .stack-work/install/x86_64-linux/lts-11.0/8.2.2/bin/rea-process rea-process
COPY stops.csv stops.csv
COPY stores.csv stores.csv

CMD ./rea-process
