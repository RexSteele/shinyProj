# Bio-Cryptex

Cryptex and Biology inspired puzzle game. Using the alteration options available via the buttons, change the player set of amino acids to get new bindings.
Continue to change the bindings until you unlock the hidden message!

## Description

The goal of the game is to change the binding sites (currently the top most line of characters) to match the secret message (currently 'GATTACCA'). You change the
b

## Getting Started

### Dependencies
  R Libraries:
    shiny

### Installing
  No installation of the game itself is needed. Install R Studio on your computer, then run the primary script (BioCryptex.R) inside of R Studio.

### Executing program

* How to run the program
  - Open BioCryptex.R in RStudio
  - Click inside the text area where the program code is displayed
  - Press Ctrl + Shift + Enter

### Buttons

  - Shuffle All : Shuffle all player values (This is also the method to start the game currently)
  - Mutate : Shuffles one random letter in a single column of the player values
  - Recombination : Shuffles a random number of contiguous columns
  - Shuffle One Selection : Option to select a specific player column, and shuffle just that column (Planned to be removed at a later date)
  - Lock Column Selection : Option to lock a specific column(s). Currently only applies to the Shuffle All action
  - Revert : Revert player values to prior set

## Authors

Rex Steele

## Acknowledgments

Inspiration, code snippets, etc.
* [Shiny from RStudio](https://shiny.rstudio.com/)
