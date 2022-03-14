#!/bin/bash

# Welcome to the Cardano Alonzo NFT Creator
#
# Contributed by Joe Totes - Plutus Pioneers Cohort 3 - https://github.com/Totes5706
#
# This script creates the validation keys and addresses, and passes the parameters through haskell/plutus validation.
# It takes the plutus policy from the validation and submits the results to the cardano-cli
# The plutus policy ensures only 1 token is minted. If there are attempts from a bad actor to alter the off-chain code, the transaction will fail
#
# Requirements:
#
# 1) Nix-shell - Using git checkout 6e3f6a59d64f6d4cd9d38bf263972adaf4f7b244 in the plutus-apps directory of the IOHK repo
# 2) Fully synced cardano node
#
# This script has not been audited! If you use this to mint NFTs on the mainnet, do so at your own risk!

echo;
echo ------------------------------------------------------
echo Welcome to the Cardano Alonzo NFT Creator!
echo ------------------------------------------------------
echo;


#get location of the cardano node from the user if it is undefined. Also check if they entered the correct directory.
if [ -z "$CARDANO_NODE_SOCKET_PATH" ]
then
    while true ; do
        read -p 'Enter the location of the cardano node socket (ex: /opt/cardano/cnode/sockets/node0.socket): ' nodepath
        if [ -e "${nodepath}" ]; then
            echo;
            echo cardano-node socket location found at: $nodepath
            export CARDANO_NODE_SOCKET_PATH=$nodepath
            break
        else
            echo "Location of node socket not found, please re-enter"
        fi
    done
else
    echo cardano-node socket location detected at: $CARDANO_NODE_SOCKET_PATH
fi
echo;

#specify from the user whether they are using either the testnet or mainnet and save into variable magic
echo Which Cardano network will you be using?

select magic in 'mainnet' 'testnet' 
do
     [[ -n $magic ]] && break || {
        echo "invalid input"
    }
done

case $magic in

    mainnet) 
        magic='--mainnet'
        ;;
    
    testnet)
        magic='--testnet-magic 1097911063' 
        ;;
esac
echo You chose: $magic;

echo;

#get the NFT name from the user
read -p 'Enter of the NFT name you want to create (no spaces or special characters allowed) : ' tn
echo;


#check for spaces and special characters
re="[[:space:]]+"
while [[ $tn = *[-@#$%'&'*=+]* ]] || [[ $tn =~ $re ]]
do
    read -p 'Token name not allowed, please enter again (no spaces or special characters allowed) : ' tn
done

#Since this is an NFT, we will only mint 1 token. Changing this value to anything greater then 1 will result in the transaction failing
#The plutus policy on-chain code acts as a validator, so any actor trying to force more than one token will cause this script to fail as intended
amt=1
echo The number of tokens that will be minted is: $amt
echo;

#create NFT file directory and change to that directory
mkdir -p NFT
cd NFT

#create a testnet or mainet folder depending on the network choice
if [ "$magic" = '--mainnet' ] 
then
    mkdir -p mainnet
    cd mainnet
else    
    mkdir -p testnet
    cd testnet
fi

#create folder for this specfic token name and change to that directory
mkdir -p $tn
cd $tn

#generate vrf keys .vkey and .skey for the transaction, asking user permission to replace if files already exist
if [ -f payment.skey ] || [ -f payment.vkey ] 
then
    echo Warning, payment.skey and payment.vkey files already exist for the token $tn . Would you like to overwrite? Make sure to backup these files before replacing them.

    select overwrite in 'yes' 'no' 
    do
        [[ -n $overwrite ]] && break || {
        echo "invalid input"
    }
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

#generate payment receive address for the transaction, asking user permission to replace if files already exist
if [ -f payment.addr ] 
then
    echo Warning, payment address payment.addr already exists for the token $tn . Would you like to overwrite? Make sure to backup these files before replacing them.

    select overwrite in 'yes' 'no' 
    do
        [[ -n $overwrite ]] && break || {
        echo "invalid input"
    }
    done

    case $overwrite in

    yes)
        echo Generating payment address into payment.addr 

        cardano-cli address build \
            --payment-verification-key-file payment.vkey \
            --out-file payment.addr $magic
        ;;
    
    no)
        echo original address file left unchanged
        ;;
    esac
else
    echo Generating payment address into payment.addr

    cardano-cli address build \
            --payment-verification-key-file payment.vkey \
            --out-file payment.addr $magic

fi
echo;

#declare variable address to be the receive address of the new key pair
address=$(cat payment.addr)

#output address to user so they can fund the wallet. Check if they are using mainnet or testnet to display the correct blockchain explorer link
echo ;echo;
echo ------------------------------------------------------
echo You are currently set up on the $magic
echo;
echo payment address = $address
echo;
echo Fund this address with ADA to get started.
echo ------------------------------------------------------
echo ;echo;

read -p "Once this address is funded, press enter to continue "
echo;
#query the CLI at the address until we see the funds have arrived
addressfunded=false
while [ $addressfunded = false ]
do
    cardano-cli query utxo \
        --address $address \
        $magic
    
    cardano-cli query utxo \
        --address $address \
        $magic \
        --out-file utxoquery.txt

    array_txid=($(awk -F'"' '/#/{print $2}' utxoquery.txt))
    
    echo;
    echo The following utxos have been found. Query the address again?

    select isfunded in 'Query address again' 'Continue to mint' 
    do
        [[ -n $isfunded ]] && break || {
        echo "invalid input"
    }
    done

    case $isfunded in

    'Query address again')
        echo;
        echo Address not funded yet, querying the blockchain again...
        echo;
        ;;
    
    'Continue to mint')
        if [ -n "$array_txid" ] 
        then
            addressfunded=true
        else 
            echo;
            echo Error! No utxos have been found at the address! Please fund the address or wait for the funds to arrive.
        fi
        ;;
    esac
    
done
echo;

#Specify from the user which utxo to use for minting
echo 'Which utxo would you like to use (enter number selection)?'
select oref in "${array_txid[@]}"
 do
    [[ -n $oref ]] && break || {
        echo "invalid input"
    }
done
echo;

#ask the user where the token would be sent
echo Would you like this NFT minted in this address, or have it transfered to another address?

select sendto in 'Keep the NFT in this address' 'Transfer the NFT to a recipient address' 
do
     [[ -n $sendto ]] && break || {
     echo "invalid input"
    }
done

case $sendto in

'Keep the NFT in this address')
    sendaddress=$address
    changeaddress=$address
    ;;
    
'Transfer the NFT to a recipient address')
    echo;
    read -p 'Enter the recipient address you want to send to (no spaces or special characters allowed): ' sendaddress
    echo;

    #check for spaces and special characters
    re="[[:space:]]+"
    while [[ $sendaddress = *[-@#$%'&'*=+]* ]] || [[ $sendaddress =~ $re ]]
    do
        read -p 'Address name not allowed, please enter again (no spaces or special characters allowed) : ' sendaddress
    done

    #ask the user if the remaining ADA be sent to the recipient
    echo 'Do you want the left over ADA (extra change) be sent to the recipient as well?'

    select change in 'Keep remaining ADA in this address' 'Transfer remaining ADA to the recipient with the NFT' 
    do
        [[ -n $change ]] && break || {
        echo "invalid input"
    }
    done

    case $change in

    'Keep remaining ADA in this address')
        changeaddress=$address
        ;;
    
    'Transfer remaining ADA to the recipient with the NFT')
        changeaddress=$sendaddress
        ;;
    esac
    ;;
esac
    

echo;

#query the protocol parameters and save them into the file protocol.json
echo Generating protocol parameters into protocol.json
echo;

cardano-cli query protocol-parameters \
    $magic \
    --out-file protocol.json

#generate unit.json file for the mint reedeemer in the transaction build
echo Generating unit.json
echo "{\"fields\":[],\"constructor\":0}" > unit.json 
echo;

#create policy file to store policy files
mkdir -p policy

#create new policy file in the new directory policy
echo Generating NFT policy 
echo;
policyFile=policy/token.plutus

#Send these four parameters to the on-chain code of Token.Onchain.hs to validate, then create the policy for the NFT
cabal exec token-policy $policyFile $oref $tn

#create a signed and unsigned file to prepare for the Cardano-CLI transaction build/sign
unsignedFile=tx.unsigned
signedFile=tx.signed

#create a policyid using the CLI
pid=$(cardano-cli transaction policyid --script-file $policyFile)

#convert the token name into hexadecimal format using haskell, so the CLI can interpet it:
tnHex=$(cabal exec token-name -- $tn)

#compute the unique minted value based off the amount, policyid, and token name
v="$amt $pid.$tnHex"

echo Token Name : $tn
echo Token Name Hex : $tnHex
echo Tokens Minted : $amt
echo UTXO : $oref
echo Address : $address
echo Recipient NFT Address: $sendaddress
echo Policy Id : $pid
echo v : $v
echo Policy File Directory : $policyFile 
echo;

#build the transaction using the parameters from above
cardano-cli transaction build \
    $magic \
    --tx-in $oref \
    --tx-in-collateral $oref \
    --tx-out "$sendaddress + 1500000 lovelace + $v" \
    --mint "$v" \
    --mint-script-file $policyFile \
    --mint-redeemer-file unit.json \
    --change-address $changeaddress \
    --protocol-params-file protocol.json \
    --out-file $unsignedFile \

#sign the transaction using the parameters from above
cardano-cli transaction sign \
    --tx-body-file $unsignedFile \
    --signing-key-file payment.skey \
    $magic \
    --out-file $signedFile

#submit the transaction
cardano-cli transaction submit \
    $magic \
    --tx-file $signedFile

echo;

read -p "Transaction has been submitted, press enter to query the local address "

#check to see if the token arrived before the script closes
nftarrived=false
while [ $nftarrived = false ]
do
    echo;
    echo Local Address: $address 
    echo;

    cardano-cli query utxo \
        --address $address \
        $magic

    echo;
    echo;
    echo Recipient Address: $sendaddress
    echo;

    cardano-cli query utxo \
        --address $sendaddress \
        $magic
    
    echo;
    echo The following utxos have been found. What would you like to do?

    select isquery in 'Query addresses again' 'Exit'
    do
        [[ -n $isquery ]] && break || {
        echo "invalid input"
    }
    done

    case $isquery in

    'Query addresses again')
        echo;
        echo Querying the blockchain again...
        echo;
        ;;

    'Exit')
        nftarrived=true
        ;;
    esac
    
done

echo Process Completed
