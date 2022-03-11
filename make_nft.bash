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

#create NFT file directory and change to that directory
mkdir -p NFT
cd NFT

#create NFT folder for this specfic NFT and change to that directory
mkdir -p $nftname
cd $nftname

#generate vrf keys .vkey and .skey for the transaction, asking user permission to replace if files already exist
if [ -f payment.skey ] || [-f payment.vkey ] 
then
    echo Vrf key files already exist. Would you like to overwrite?
    select overwrite in 'yes' 'no' 
    do
        break
    done

    case $overwrite in

    yes) 
        echo Generating payment.vkey and payment.skey
        cardano-cli address key-gen --verification-key-file payment.vkey --signing-key-file payment.skey 
        ;;
    
    no)
        echo Original vrf files left unchanged
        ;;
    esac
else 
    echo Generating payment.vkey and payment.skey
    cardano-cli address key-gen --verification-key-file payment.vkey --signing-key-file payment.skey 

fi


#generate payment recieve address for the transaction, asking user permission to replace if files already exist
if [ -f payment.addr ] 
then
    echo Payment address already exists. Would you like to overwrite?
    select overwrite in 'yes' 'no' 
    do
        break
    done

    case $overwrite in

    yes)
        echo Generating payment.addr 
        cardano-cli address build --payment-verification-key-file payment.vkey --out-file payment.addr $NETWORK
        ;;
    
    no)
        echo original address file left unchanged
        ;;
    esac
else
    echo Generating payment.addr
    cardano-cli address build --payment-verification-key-file payment.vkey --out-file payment.addr $NETWORK

fi

#declare variable address - to be the recieve address of the newly created keys
address=$(cat payment.addr)

#output address to user so they can fund the wallet. Check if they are using mainnet or testnet to display the correct blockchain explorer link
echo ;echo;
echo ------------------------------------------------------
echo payment address = $address
echo;
echo Fund this address with ADA to get started.
echo ------------------------------------------------------
echo ;echo;

#query the CLI at the address until we see the funds have arrived
addressfunded=false
while [ $addressfunded == false ]
do
    cardano-cli query utxo --address $address $NETWORK
    echo;
    echo Is the address funded yet?
    select isfunded in 'yes' 'no' 
    do
        break
    done

    case $isfunded in

    yes)
        addressfunded=true
        ;;
    
    no)
        echo Address not funded yet, querying the blockchain again...
        echo;
        ;;
    esac
    
done

read -p "If the address contains enough ADA, Press enter to continue : "
