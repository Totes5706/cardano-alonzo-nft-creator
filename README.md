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
  - [How to Use the NFT Creator](#how-to-use-the-nft-creator)
  - [Sample Output](#sample-output)



## Pre-requisites Required

In order to run the bash script, two dependencies are needed:

- nix needs to be installed in order to use the proper git commit for nix-shell. This can be run without nix, however it is highly recommended and the easier path to setup.
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



## How to Use the NFT Creator

First, make sure your cardano-node is running and fully synced. 
Also, make note of the location node.socket file for your node; you will need the directory location of it when you start the script.
Alternatively, in the [env.sh](https://github.com/Totes5706/cardano-alonzo-nft-creator/blob/main/env.sh) you can instead include the path there and then run . ```env.sh``` to declare it as an environment variable.

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


- Directory: ```[nix-shell:~/cardano-alonzo-nft-creator]$```
```
./make-nft.bash
```

```
------------------------------------------------------
Welcome to the Cardano Alonzo NFT Creator!
------------------------------------------------------

cardano-node socket location detected at: /home/totinj/DriveTwo/cardano/cnode/sockets/node0.socket

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

payment address = addr_test1vpggyzeqrc5rmwa98lk728920xj79f4pshtvr9rc3pcwpwgchsae9

Fund this address with ADA to get started.
------------------------------------------------------


Once this address is funded, press enter to continue 
```
Fund the address from either another wallet or the faucet (if you are on the testnet)

![Screenshot 2022-03-14 at 19-34-33 Faucet](https://user-images.githubusercontent.com/59018247/158277931-de09365b-c275-4be3-b484-8bd90ce2955c.png)


```
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------

The following utxos have been found. Query the address again?
1) Query address again
2) Continue to mint
#? 1
```

```
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
c333dd410bb7b7b4a124e6ee7383ca6df7fe0b76e275347c9180f0c1845bfd82     0        1000000000 lovelace + TxOutDatumNone

The following utxos have been found. Query the address again?
1) Query address again
2) Continue to mint
#? 2
```


```
Which utxo would you like to use (enter number selection)?
1) c333dd410bb7b7b4a124e6ee7383ca6df7fe0b76e275347c9180f0c1845bfd82#0
#? 1
```

```
Do you want the NFT minted in this address, or have it transfered to another address?
1) Keep the NFT in this address
2) Transfer the NFT to a recipient address
#? 2
```

```
Enter the recipient address you want to send to (no spaces or special characters allowed):
addr_test1qp080kw89tmt5jmp3m54qnu5edxfjyh966ylezf0e2syzcw7h0jw8jsptmz6zgv45kzmhf9gn6l75t0xjkz6rlt69qzqffjzds
```

```
Do you want the left over ADA (extra change) be sent to the recipient as well?
1) Keep remaining ADA in this address
2) Transfer remaining ADA to the recipient with the NFT
#? 2
```

```
Generating protocol parameters into protocol.json

Generating unit.json

Generating NFT policy

Token Name : IOHK
Token Name Hex : 494f484b
Tokens Minted : 1
UTXO : c333dd410bb7b7b4a124e6ee7383ca6df7fe0b76e275347c9180f0c1845bfd82#0
Address : addr_test1vpggyzeqrc5rmwa98lk728920xj79f4pshtvr9rc3pcwpwgchsae9
Recipient NFT Address: addr_test1qp080kw89tmt5jmp3m54qnu5edxfjyh966ylezf0e2syzcw7h0jw8jsptmz6zgv45kzmhf9gn6l75t0xjkz6rlt69qzqffjzds
Policy Id : c972ebee7efd168b6bded450213b5eeb03021a96032eeee3f38e2890
v : 1 c972ebee7efd168b6bded450213b5eeb03021a96032eeee3f38e2890.494f484b
Policy File Directory : policy/token.plutus

Estimated transaction fee: Lovelace 348460
Transaction successfully submitted.

Transaction has been submitted, press enter to query the local address 
```

```
Transaction has been submitted, press enter to query the local address 

Local Address: addr_test1vpggyzeqrc5rmwa98lk728920xj79f4pshtvr9rc3pcwpwgchsae9

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------





Recipient Address: addr_test1qp080kw89tmt5jmp3m54qnu5edxfjyh966ylezf0e2syzcw7h0jw8jsptmz6zgv45kzmhf9gn6l75t0xjkz6rlt69qzqffjzds

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
bf5571cf9caddb3f9be3b4328f7f0815e86009d4c37c878630ded99e0556dbd8     0        998151540 lovelace + TxOutDatumNone
bf5571cf9caddb3f9be3b4328f7f0815e86009d4c37c878630ded99e0556dbd8     1        1500000 lovelace + 1 c972ebee7efd168b6bded450213b5eeb03021a96032eeee3f38e2890.494f484b + TxOutDatumNone

The following utxos have been found. What would you like to do?
1) Query addresses again
2) Exit
#? 2
```

![Screenshot 2022-03-14 at 19-45-18 Address addr_test1qp080kw89tmt5jmp3m54qnu5edxfjyh966ylezf0e2syzcw7h0jw8jsptmz6zgv45kzmhf9gn6l75t0xjkz6rlt69qzqffjzds - Cardanoscan](https://user-images.githubusercontent.com/59018247/158278765-c33e2a94-4a39-4f94-a138-07178cbd84d0.png)
 
 
 ![Screenshot 2022-03-14 at 19-45-55 Token IOHK - Cardanoscan](https://user-images.githubusercontent.com/59018247/158278819-fb32c8d3-1167-42f4-a47c-372887ffd548.png)


![Screenshot 2022-03-14 at 19-46-44 Transaction bf5571cf9caddb3f9be3b4328f7f0815e86009d4c37c878630ded99e0556dbd8 - Cardanoscan](https://user-images.githubusercontent.com/59018247/158278884-ddf1067b-f821-40e8-9b67-1804c25ecb94.png)



