#! /usr/bin/bash
cardano-node run --config $HOME/cardano-src/testnet-config.json \
--database-path $HOME/cardano-src/db/ \
--socket-path $HOME/cardano-src/db/node.socket \
--host-addr 127.0.0.1 \
--port 1337 \
--topology $HOME/cardano-src/testnet-topology.json