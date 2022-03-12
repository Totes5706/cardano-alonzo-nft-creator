#!/bin/bash

echo;
#get location of the cardano node from the user if it is undefined
if [ -z "$CARDANO_NODE_SOCKET_PATH" ]
then
    read -p 'Enter the location of the cardano node socket (ex: /opt/cardano/cnode/sockets/node0.socket): ' nodepath
    #declare location of the node as an environment variable
    export CARDANO_NODE_SOCKET_PATH=$nodepath
else
    echo cardano-node socket location detected at: $CARDANO_NODE_SOCKET_PATH
fi
echo;

#specify from the user whether they are using either the testnet or mainnet and save into variable magic
echo Which Cardano network will you be using?

select magic in 'mainnet' 'testnet' 
do
    break
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

#Since this is an NFT, we will only mint 1 token
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

#generate payment receive address for the transaction, asking user permission to replace if files already exist
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

#declare variable address a- to be the receive address of the newly created keys
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

#query the CLI at the address until we see the funds have arrived
addressfunded=false
while [ $addressfunded = false ]
do
    cardano-cli query utxo \
        --address $address \
        $magic
    echo;
    echo Has the ADA appeared above inside the utxo?

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
echo;

#store utxo information to file
cardano-cli query utxo \
        --address $address \
        $magic \
        --out-file utxoquery.txt

#store the query utxo in an array, so we can prompt the user to select one
array_txid=($(awk -F'"' '/#/{print $2}' utxoquery.txt))

#Specify from the user which utxo to use for minting
echo Which utxo would you like to use?
select oref in "${array_txid[@]}"
 do
    break
done
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
echo Generating NFT policy using the following parameters:
echo;

policyFile=policy/token.plutus

echo Token Name : $tn
echo Tokens Minted : $amt
echo UTXO : $oref
echo Address : $address
echo Policy File Directory : $policyFile 
echo;

#Send these four parameters to the on-chain code of Token.Onchain.hs to create the policy for the NFT
cabal exec token-policy $policyFile $oref $amt $tn

#create a signed and unsigned file to prepare for the Cardano-CLI transaction build/sign
unsignedFile=tx.unsigned
signedFile=tx.signed

#create a policyid using the CLI
pid=$(cardano-cli transaction policyid --script-file $policyFile)

#convert the Token Name into hexadecimal format so the CLI can interpet it:
tnHex=$(cabal exec token-name -- $tn)

#compute the unique minted value based off the amount, policyid, and token name
v="$amt $pid.$tnHex"

#Build the transaction using the parameters from above
cardano-cli transaction build \
    $magic \
    --tx-in $oref \
    --tx-in-collateral $oref \
    --tx-out "$address + 1500000 lovelace + $v" \
    --mint "$v" \
    --mint-script-file $policyFile \
    --mint-redeemer-file unit.json \
    --change-address $address \
    --protocol-params-file protocol.json \
    --out-file $unsignedFile \

#Sign the transaction using the parameters from above
cardano-cli transaction sign \
    --tx-body-file $unsignedFile \
    --signing-key-file payment.skey \
    $magic \
    --out-file $signedFile

#Submit the transaction
cardano-cli transaction submit \
    $magic \
    --tx-file $signedFile

#check to see if the tokens arrived before the script closes
tokenfunded=false
while [ $tokenfunded = false ]
do
    cardano-cli query utxo \
        --address $address \
        $magic
    echo;
    echo Has the token arrived in the wallet above?

    select istoken in 'yes' 'no' 
    do
        break
    done

    case $istoken in

    yes)
        tokenfunded=true
        
        ;;
    
    no)
        echo;
        echo token has not arrived, querying the blockchain again...
        echo;
        ;;
    esac
    
done