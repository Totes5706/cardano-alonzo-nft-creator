# Cardano Alonzo NFT Creator

## Overview

In this project, we will be using the bash script [make-nft.bash](https://github.com/Totes5706/cardano-alonzo-nft-creator/blob/main/make-nft.bash) to mint NFTs fast and easy without requiring the Plutus Application Backend (PAB). 

This script creates the validation keys and addresses, and passes the parameters through haskell/plutus validation to create a minting policy. It then takes the plutus policy from the validation and submits the results to the cardano-cli. The plutus policy ensures only 1 token is minted. If there are attempts from a bad actor to alter the off-chain code, the transaction will fail. Time deadlines are not needed for this implementation.

## Table of Contents

- [Cardano Alonzo NFT Creator](#cardano-alonzo-nft-creator)
  - [Overview](#overview)
  - [Pre-requisites Required](#pre-requisites-required)
    - [Nix Install on Linux](#nix-install-on-linux)
    - [Syncing the Cardano Node](#syncing-the-cardano-node)
  - [How to Use the NFT Maker](#how-to-use-the-nft-maker)
  - [Sample Output](#sample-output)



## Pre-requisites Required

In order to run the bash script, two dependencies are needed:

- nix needs to be installed in order to use the proper git commit for nix-shell
- the cardano node must be fully synced

If you are in the Plutus Pioneer Program, these should already be installed. If not, follow along the next two subsections for the how to installation.

### Nix Install on Linux

First, Open up the terminal to get started. We will first install the necessary dependencies first for a fresh copy of Linux.

We need to install Nix and get it configured properly to use IOG’s caches. In this guide we will be doing a single user install.
Before we can install Nix, we need to make sure the version of Linux you are using has both curl and git installed. First run:

- Directory: ```totinj@penguin:~$```
```
sudo sh -c 'apt update && apt install curl'
```


Now that curl is installed, we can install git. Run:

- Directory: ```totinj@penguin:~$```
```
sudo apt-get install git
```

We can now install Nix single user install. Run:

- Directory: ```totinj@penguin:~$```
```
sh <(curl -L https://nixos.org/nix/install) --no-daemon
```

Now to finish, we need to set the environment with the following command notice from above.
Very important here to replace ```totinj``` with your current Linux user!!

- Directory: ```totinj@penguin:~$```
```
. /home/totinj/.nix-profile/etc/profile.d/nix.sh
```


We now need to add Input Outputs caches to greatly speed up the building process. Without this step, you might be running nix-shell for days rather than minutes! This can be found here: [IOG Binaries](https://github.com/input-output-hk/plutus-apps#iohk-binary-cache). Let’s create a new config file that has the associated IOG links. Run:

- Directory: ```totinj@penguin:~$```
```
mkdir ~/.config/nix
echo 'substituters = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/' >> ~/.config/nix/nix.conf
echo 'trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=' >> ~/.config/nix/nix.conf
```
This part should now be completed.

### Syncing the Cardano Node

By default, nix-shell includes the cardano-node and the cardano-cli. Therefore, we need to run nix-shell then sync the node.

First, let’s clone plutus-apps repo from IOHK:

- Directory: ```totinj@penguin:~$```
```
git clone https://github.com/input-output-hk/plutus-apps.git
```

Next, let’s clone this repo:

- Directory: ```totinj@penguin:~$```
```
git clone https://github.com/Totes5706/cardano-alonzo-nft-creator.git
```

Head to the plutus-apps directory and update it to the current git tag:

- Directory: ```totinj@penguin:~/plutus-apps$```
```
git checkout 6e3f6a59d64f6d4cd9d38bf263972adaf4f7b244
```

You should now be on the proper git branch and can run nix-shell in this directory. Run nix-shell:


- Directory: ```totinj@penguin:~/plutus-apps$```
``` 
nix-shell
```

If this is run for the first time, it will take some time to build (30min + for a non-workstation computer).

While in nix-shell, head to the node directory inside this repo that we just cloned:


- Directory: ```[nix-shell:~/cardano-alonzo-nft-creator/node]$```

Here, you can now sync either the mainnet or testnet using the scripts:

- Directory: ```[nix-shell:~/cardano-alonzo-nft-creator/node]$```
```
./start-testnet-node
```


- Directory: ```[nix-shell:~/cardano-alonzo-nft-creator/node]$```
```
./start-mainet-node
```



## How to Use the NFT Maker

First, make sure your cardano-node is running and fully synced.

Second, ensure you have plutus-apps repo from IOHK cloned and also this repo.
If you do not, clone plutus-apps repo from IOHK by the following:

- Directory: ```totinj@penguin:~$```
```
git clone https://github.com/input-output-hk/plutus-apps.git
```

Clone this repo by:

- Directory: ```totinj@penguin:~$```
```
git clone https://github.com/Totes5706/cardano-alonzo-nft-creator.git
```

Head to the plutus-apps directory and update it to the current git tag:

- Directory: ```totinj@penguin:~/plutus-apps$```
```  
git checkout main
```


- Directory: ```totinj@penguin:~/plutus-apps$```
```     
git pull
```

- Directory: ```totinj@penguin:~/plutus-apps$```
```
git checkout 6e3f6a59d64f6d4cd9d38bf263972adaf4f7b244
```

You should now be on the proper git branch and can run nix-shell in this directory. Run nix-shell:


- Directory: ```totinj@penguin:~/plutus-apps$```
```
nix-shell
```

Now, while in nix-shell, head over to this repo to build the project:

- Directory: ```[nix-shell:~/cardano-alonzo-nft-creator]$```
```
cabal update
```


- Directory: ```[nix-shell:~/cardano-alonzo-nft-creator]$```
```
cabal build
```

If successful, you should see the output:

```
Build profile: -w ghc-8.10.4.20210212 -O1
In order, the following will be built (use -v for more details):
 - cardano-alonzo-nft-creator-0.1.0.0 (lib) (file src/Utils.hs changed)
 - cardano-alonzo-nft-creator-0.1.0.0 (exe:token-policy) (dependency rebuilt)
 - cardano-alonzo-nft-creator-0.1.0.0 (exe:token-name) (dependency rebuilt)
Preprocessing library for cardano-alonzo-nft-creator-0.1.0.0..
Building library for cardano-alonzo-nft-creator-0.1.0.0..
[2 of 2] Compiling Utils            ( src/Utils.hs, /home/totinj/DriveTwo/cardano/git/cardano-alonzo-nft-creator/dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/cardano-alonzo-nft-creator-0.1.0.0/build/Utils.o, /home/totinj/DriveTwo/cardano/git/cardano-alonzo-nft-creator/dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/cardano-alonzo-nft-creator-0.1.0.0/build/Utils.dyn_o )
Preprocessing executable 'token-policy' for cardano-alonzo-nft-creator-0.1.0.0..
Building executable 'token-policy' for cardano-alonzo-nft-creator-0.1.0.0..
Preprocessing executable 'token-name' for cardano-alonzo-nft-creator-0.1.0.0..
Building executable 'token-name' for cardano-alonzo-nft-creator-0.1.0.0..
```
We are now ready to start using the NFT maker! To start the script, in this directory run:


- Directory: ```[nix-shell:~/cardano-alonzo-nft-creator]$```
```
./make-nft.bash
```

## Sample Output

First making sure the node is synced:

![Screenshot from 2022-03-13 21-31-50](https://user-images.githubusercontent.com/59018247/158090008-4adf91cc-2711-4987-a49f-d314c39c0f81.png)


- Directory: ```[nix-shell:~/cardano-alonzo-nft-creator]$```
```
./make-nft.bash
```

```
Enter the location of the cardano node socket (ex: /opt/cardano/cnode/sockets/node0.socket): 
/home/totinj/DriveTwo/cardano/cnode/sockets/node0.socket
```

```
Which Cardano network will you be using?
1) mainnet
2) testnet
#? 2
```

```
You chose: --testnet-magic 1097911063

Enter of the NFT name you want to create (no spaces or special characters allowed) : IOHK
```

```
The number of tokens that will be minted is: 1

Generating payment.vkey and payment.skey files


Generating payment address into payment.addr



------------------------------------------------------
You are currently set up on the --testnet-magic 1097911063

payment address = addr_test1vzs70ggma9r8mnts9hf2h7vy0wj5s7w8lr67tkhgwjmqy6qmlkeq3

Fund this address with ADA to get started.
------------------------------------------------------


Once this address is funded, press enter to continue 
```

![Screenshot 2022-03-13 at 21-40-09 Faucet](https://user-images.githubusercontent.com/59018247/158090547-c60c5add-da1d-4317-9e82-0aeab9b3eaaa.png)


```
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
bfb661a52c4ccca70722396de02b14746327312235ab3990bcccf62ea0496ee7     0        1000000000 lovelace + TxOutDatumNone

Has the ADA appeared in the address above, inside the utxo?
1) yes
2) no
#? 1
```

```
Which utxo would you like to use?
1) bfb661a52c4ccca70722396de02b14746327312235ab3990bcccf62ea0496ee7#0
#? 1
```

```
Generating protocol parameters into protocol.json

Generating unit.json

Generating NFT policy using the following parameters:

Token Name : IOHK
Tokens Minted : 1
UTXO : bfb661a52c4ccca70722396de02b14746327312235ab3990bcccf62ea0496ee7#0
Address : addr_test1vzs70ggma9r8mnts9hf2h7vy0wj5s7w8lr67tkhgwjmqy6qmlkeq3
Policy File Directory : policy/token.plutus

Estimated transaction fee: Lovelace 343314
Transaction successfully submitted.
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
bfb661a52c4ccca70722396de02b14746327312235ab3990bcccf62ea0496ee7     0        1000000000 lovelace + TxOutDatumNone

Has the token arrived in the wallet above?
1) yes
2) no
#? 2
```

```
token has not arrived, querying the blockchain again...

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2bb7b1d677c9dc5345181a5cd3851802828f7b2e31301abd3b731fd14c2f4a56     0        998156686 lovelace + TxOutDatumNone
2bb7b1d677c9dc5345181a5cd3851802828f7b2e31301abd3b731fd14c2f4a56     1        1500000 lovelace + 1 af5c529e0b6290c6960122efd5beec67ea0ebd075ccbb91c9d3d27aa.494f484b + TxOutDatumNone

Has the token arrived in the wallet above?
1) yes
2) no
#? 1
```

![Screenshot 2022-03-13 at 21-48-46 Address addr_test1vzs70ggma9r8mnts9hf2h7vy0wj5s7w8lr67tkhgwjmqy6qmlkeq3 - Cardanoscan](https://user-images.githubusercontent.com/59018247/158091239-62ba23a2-16c3-4350-b686-f1d2074a4182.png)

![Screenshot 2022-03-13 at 21-49-03 Token IOHK - Cardanoscan](https://user-images.githubusercontent.com/59018247/158091274-5fe1a724-f615-47fb-b4b7-ede576a79bbf.png)

![Screenshot 2022-03-13 at 21-49-53 Transaction 2bb7b1d677c9dc5345181a5cd3851802828f7b2e31301abd3b731fd14c2f4a56 - Cardanoscan](https://user-images.githubusercontent.com/59018247/158091282-cae27167-5979-4edf-99d9-7af8efb80ed4.png)




