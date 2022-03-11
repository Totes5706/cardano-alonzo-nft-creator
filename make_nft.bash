#!/bin/bash

read -p 'Enter the location of the cardano node socket (ex: /opt/cardano/cnode/sockets/node0.socket): ' nodepath

export CARDANO_NODE_SOCKET_PATH=$nodepath