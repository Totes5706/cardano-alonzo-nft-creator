# Cardano Alonzo NFT Creator

## Table of Contents

- [Cardano Alonzo NFT Creator](#cardano-alonzo-nft-creator)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
  - [Prerequisites Required](#prerequisites-required)
  - [How to Use the NFT Maker](#how-to-use-the-nft-maker)
  - [Sample Output](#sample-output)

## Overview

In this project, we will be using the bash script make-nft.bash. This script creates the validation keys and addresses, and passes the parameters through haskell/plutus validation to create a minting policy. It then takes the plutus policy from the validation and submits the results to the cardano-cli. The plutus policy ensures only 1 token is minted. If there are attempts from a bad actor to alter the off-chain code, the transaction will fail. Time deadlines are not needed for this implementation.

## Prerequisites Required

## How to Use the NFT Maker

## Sample Output
