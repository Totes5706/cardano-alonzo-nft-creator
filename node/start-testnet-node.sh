#!/bin/bash

cardano-node run \
    --config testnet/testnet-config.json \
    --topology testnet/testnet-topology.json \
    --database-path testnet/db \
    --socket-path testnet/node0.socket \
    --port 3003
