#! /usr/bin/bash

# cardano-cli transaction build \
# --babbage-era \
# --testnet-magic 1097911063 \
# --tx-in <UTXO> \ TxHash#TxIx
# --tx-out <Testnet Address>+<Number of Lovelace> \
# --change-address <Testnet Address> \
# --out-file tx.draft

cardano-cli transaction build \
--babbage-era \
--testnet-magic 1097911063 \
--tx-in 0029025de7f53c1b2d33fc0f0f1fcf2d916925b3e74cdbe7142881e74279e6f6#0 \
--tx-out $(cat testnet-wallet-01/payment.addr)+20000000 \
--change-address $(cat testnet-wallet-01/payment.addr) \
--out-file tx.draft

# Sign a Transaction

cardano-cli transaction sign \
--tx-body-file tx.draft \
--signing-key-file $(cat testnet-wallet-01/payment.skey) \
--testnet-magic 1097911063 \
--out-file tx.signed

# Submit a Transaction

cardano-cli transaction submit \
--testnet-magic 1097911063 \
--tx-file tx.signed