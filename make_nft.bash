#!/bin/bash

echo;
#get location of the cardano node from the user
read -p 'Enter the location of the cardano node socket (ex: /opt/cardano/cnode/sockets/node0.socket): ' NODEPATH
echo;

#declare location of the node as an environment variable
export CARDANO_NODE_SOCKET_PATH=$NODEPATH

#specify from the user whether they are using either the testnet or mainnet and declare 
echo Which Cardano network will you be using?

select network in 'mainnet' 'testnet' 
do
    break
done

case $network in

    mainnet) 
        network='--mainnet'
        ;;
    
    testnet)
        network='--testnet-magic 1097911063' 
        ;;
esac
echo You chose: $network;
echo;

#get the NFT name and IPFS CID location from the user
read -p 'Enter of the NFT name you want to create (no spaces) : ' nftname
echo;
read -p 'Enter the unique IPFS CID associated with the NFT : ' ipfs_cid
echo;

#create NFT file directory and change to that directory
mkdir -p NFT
cd NFT

#create NFT folder for this specfic NFT and change to that directory
mkdir -p $nftname
cd $nftname

#generate vrf keys .vkey and .skey for the transaction, asking user permission to replace if files already exist
if [ -f payment.skey ] || [-f payment.vkey ] 
then
    echo VRF key files already exist. Would you like to overwrite?

    select overwrite in 'yes' 'no' 
    do
        break
    done

    case $overwrite in

    yes) 
        echo Generating payment.vkey and payment.skey files
        
        cardano-cli address key-gen \
            --verification-key-file payment.vkey \
            --signing-key-file payment.skey 
        ;;
    
    no)
        echo Original vrf files left unchanged
        ;;
    esac
else 
    echo Generating payment.vkey and payment.skey files
    echo;
    cardano-cli address key-gen \
        --verification-key-file payment.vkey \
        --signing-key-file payment.skey 

fi
echo;

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
        echo Generating payment address into payment.addr 

        cardano-cli address build \
            --payment-verification-key-file payment.vkey \
            --out-file payment.addr $network
        ;;
    
    no)
        echo original address file left unchanged
        ;;
    esac
else
    echo Generating payment address into payment.addr

    cardano-cli address build \
            --payment-verification-key-file payment.vkey \
            --out-file payment.addr $network

fi
echo;

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
    cardano-cli query utxo \
        --address $address \
        $network
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
        echo;
        echo Address not funded yet, querying the blockchain again...
        echo;
        ;;
    esac
    
done

read -p "If the address contains enough ADA, Press enter to continue : "
echo;

#store utxo information to file
cardano-cli query utxo \
        --address $address \
        $network \
        --out-file utxoquery.txt

#store the query information in two arrays, one for the TxId,TxIx; the second for the ADA balance
#awk -F'"' '/#/{print $2}' utxoquery.txt
array_txid=($(awk -F'"' '/#/{print $2}' utxoquery.txt))
#array_lovelace = (awk -F'"' '/#/{print $2}' utxoquery.txt)

#combine them into one array
#for idx in "${!array_txid[@]}"; do 
#    array_utxo[idx]=$(( array_txid[idx] + array_lovelace[idx] ))
#done


#Specify from the user which utxo to use
echo Which utxo would you like to use?
select txidix in "${array_txid[@]}".
 do
    break
done
echo;

#query the protocol parameters and save them into the file protocol.json
echo Generating protocol parameters into protocol.json
echo;

cardano-cli query protocol-parameters \
    $network \
    --out-file protocol.json

#generate the policyID files into a new folder called policy, asking user permission to replace if files already exist
mkdir -p policy

if [ -f "policy/policy.skey" ] || [-f "policy/policy.vkey" ] 
then
    echo Policy key files already exist. Would you like to overwrite?

    select overwrite in 'yes' 'no' 
    do
        break
    done

    case $overwrite in

    yes) 
        echo Generating policy/policy.vkey and policy/policy.skey files

        cardano-cli address key-gen \
            --verification-key-file policy/policy.vkey \
            --signing-key-file policy/policy.skey 
        ;;
    
    no)
        echo Original policy files left unchanged
        ;;
    esac
else 
    echo Generating payment.vkey and payment.skey files

    cardano-cli address key-gen \
        --verification-key-file policy/policy.vkey \
        --signing-key-file policy/policy.skey  
fi
echo;

# Extract uto - awk -F'"' '/#/{print $2}' query.txt 
# Extact address - awk -F'"' '/address/{ print $4}' query.txt
# Extract ADA value - awk '/lovelace/{ print $2}' query.txt

# array=( $(awk -F ' ' '{ print $NF }' log filename) )
