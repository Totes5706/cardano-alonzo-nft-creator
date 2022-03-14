#!/bin/bash

cardano-node run \
    --config mainnet/mainnet-config.json \
    --topology mainnet/mainnet-topology.json \
    --database-path mainnet/db \
    --socket-path mainnet/node0.socket \
    --port 3003
