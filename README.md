# Game Overview

This is a game where you can trade in the fun trinkets you collected for 100% real gold (tokens)! All you have to do is submit your trinkets to the "mint" endpoint and you will receive an equal value of goldTokens.

# Development Notes

Because this is a game, it is assumed that the gold tokens can be minted as needed and trinkets are burned when traded for gold tokens. Game trinkets would be created by the trinket minting policy, but the requirements for minting trinkets is outside the scope of this project. Therefore the trinket minting policy allows only burning and the trinkets are arbitrarily created in an emulator config.

This program is not limited by the number of UTxOs sitting at a script address, so the only limit on the number of concurrent trinket -> token conversions is the block size.

I was unable to get the project to compile with the intended variables for inputsBurned and outputGoldCorrect. They have been replaced with dummy variables to get it to build, though the minting policy allows gold tokens to be arbitrarily minted and burned with the dummy variables. The code that is commented out is the intended functionality described in the comment above it.
