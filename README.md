# Cardano Alonzo NFT Creator

## Overview

In this project, we will be using the bash script [make-nft.bash](https://github.com/Totes5706/cardano-alonzo-nft-creator/blob/main/make-nft.bash) to mint NFTs fast and easy without requiring the Plutus Appplication Backend (PAB). 

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

- nix needs to be installed in order to use the proper git commit for nix shell
- the cardano node must be fully synced

If you are in the Plutus Pioneer Program, these should already be installed. If not, follow along the next two subsections for the how to installation.

### Nix Install on Linux

First, Open up the terminal to get started. We will first install the necessary dependencies first for a fresh copy of Linux.

We need to install Nix and get it configured properly to use IOG’s caches. In this guide we will be doing a single user install.
Before we can install Nix, we need to make sure the version of Linux you are using has both curl and git installed. First run:

```
totinj@penguin:~$ sudo sh -c 'apt update && apt install curl'
```


Now that curl is installed, we can install git. Run:

```
totinj@penguin:~$ sudo apt-get install git
```

We can now install Nix single user install. Run:

```
totinj@penguin:~$ sh <(curl -L https://nixos.org/nix/install) --no-daemon
```


```
Output:
Installation finished!  To ensure that the necessary environment
variables are set, either log in again, or type
  . /home/totinj/.nix-profile/etc/profile.d/nix.sh
```


Now to finish, we need to set the environment with the following command notice from above.
Very important here to replace ```totinj``` with your current Linux user!!
```
totinj@penguin:~$ . /home/totinj/.nix-profile/etc/profile.d/nix.sh
```


We now need to add Input Outputs caches to greatly speed up the building process. Without this step, you might be running nix-shell for days rather than minutes! This can be found here: [IOG Binaries](https://github.com/input-output-hk/plutus-apps#iohk-binary-cache). Let’s create a new config file that has the associated IOG links. Run:
```
totinj@penguin:~$ mkdir ~/.config/nix
echo 'substituters = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/' >> ~/.config/nix/nix.conf
echo 'trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=' >> ~/.config/nix/nix.conf
```
This part should now be completed.

### Syncing the Cardano Node

By default, nix-shell includes the cardano-node and the cardano-cli. Therefore, we need to run nix-shell then sync the node.

First, let’s clone plutus-apps repo from IOHK:
```
totinj@penguin:~$ git clone https://github.com/input-output-hk/plutus-apps.git
```
Next, let’s clone this repo:
```
totinj@penguin:~$ git clone https://github.com/Totes5706/cardano-alonzo-nft-creator.git
```

## How to Use the NFT Maker

## Sample Output
