#!/bin/bash

#get location of the cardano node from the user
read -p 'Enter the location of the cardano node socket (ex: /opt/cardano/cnode/sockets/node0.socket): ' nodepath 

#declare location of the node as an environment variable
export CARDANO_NODE_SOCKET_PATH=$nodepath

#specify from the user whether they are using either the testnet or mainnet and declare 
echo Which Cardano network will you be using?

select NETWORK in 'mainnet' 'testnet' 
do
    break
done

case $NETWORK in

    mainnet) 
        NETWORK='--mainnet'
        ;;
    
    testnet)
        NETWORK='--testnet-magic 1097911063' 
        ;;
esac

#get the NFT name and IPFS CID location from the user
read -p 'Enter of the NFT name you want to create (no spaces) : ' nftname
read -p 'Enter the unique IPFS CID associated with the NFT : ' ipfs_cid

#change to NFT directory and create new folder with the NFT name
cd NFT
mkdir -p $nftname

#change to newly created token name folder
cd $nftname

#generate vrf keys .vkey and .skey for the transaction, asking user permission to replace if files already exist
if [ -f payment.skey ] || [-f payment.vkey ] 
then
    echo "Vrf key files already exist. Would you like to overwrite?"
    select overwrite in 'yes' 'no' 
    do
        break
    done

    case $overwrite in

    yes) 
        cardano-cli address key-gen --verification-key-file payment.vkey --signing-key-file payment.skey 
        ;;
    
    no)
        echo "Original vrf files left unchanged"
        ;;
    esac
else 
    cardano-cli address key-gen --verification-key-file payment.vkey --signing-key-file payment.skey 

fi


#generate payment recieve address for the transaction, asking user permission to replace if files already exist
if [ -f payment.addr ] 
then
    echo "Payment address already exists. Would you like to overwrite?"
    select overwrite in 'yes' 'no' 
    do
        break
    done

    case $overwrite in

    yes) 
        cardano-cli address build --payment-verification-key-file payment.vkey --out-file payment.addr $NETWORK
        ;;
    
    no)
        echo "original address file left unchanged"
        ;;
    esac
else 
    cardano-cli address build --payment-verification-key-file payment.vkey --out-file payment.addr $NETWORK

fi