#!/bin/bash

cardano-node run \
    --config mainnet/mainnet-config.json \
    --topology mainnet/mainnet-topology.json \
    --database-path mainnet/db \
    --socket-path mainnet/node.sock \
    --port 3003
