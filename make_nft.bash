#!/bin/bash

#get location of the cardano node from the user
read -p 'Enter the location of the cardano node socket (ex: /opt/cardano/cnode/sockets/node0.socket): ' nodepath 

#declare location of the node as an environment variable
export CARDANO_NODE_SOCKET_PATH=$nodepath

#specify from the user whether they are using either the testnet or mainnet and declare 
echo Which Cardano network will you be using?

select MAGIC in 'mainnet' 'testnet' 
do
    break
done

case $NETWORK in

    mainnet) 
        MAGIC='mainnet'
        ;;
    
    testnet)
        MAGIC='testnet-magic 1097911063' 
        ;;
esac

#get the NFT name and IPFS CID location from the user
read -p 'Enter of the NFT want to create (no spaces) : ' nftname
read -p 'Enter the unique IPFS CID associated with the NFT : ' ipfs_cid

#change to NFT directory and create new folder with the NFT name
cd NFT
mkdir -p $nftname

#change to newly created token name folder
cd $nftname