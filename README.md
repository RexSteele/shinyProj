# Bio-Cryptex

Cryptex and Biology inspired puzzle game. Using the alteration options available via the buttons, change the player set of amino acids to get new bindings.
Continue to change the bindings until you unlock the hidden message!

## Description

The goal of the game is to change the binding sites, the top most line of characters, to match the secret key(s). The initial key is set to "GATTACCA". Additional keys, up to 5 in total, can be selected at start of game. Additional keys will be randomly generated.

Changes to the player values will be denoted with red text. The score values listed below the Cryptex are the 'adist()' of values from the secret key.

Once the game has reached the end state a modal will load, giving the option to quit or start a new game. The game values up to this point will be uploaded to a dropbox account afterwards, allowing statistical analysis of all runs to date.

## Getting Started

### Dependencies
  R Libraries:
    shiny
    tidyverse
    rdrop2

### Installing
  No installation of the game itself is needed. Install R Studio on your computer, install the requisite library modules, then run the primary script (BioCryptex.R) inside of R Studio.
  
 * A Note on Dropbox
    
    The rdrop2 library requires a token to enable uploads to the DropBox account. Set up an account, and place that token in the main directory of this project. For information this see : [rdrop2 documentation](https://cran.r-project.org/web/packages/rdrop2/rdrop2.pdf)

### Executing program

* How to run the program
  - Open BioCryptex.R in RStudio
  - Click inside the text area where the program code is displayed
  - Press Ctrl + Shift + Enter


### Uploading to Shiny Server

To upload to a shinyapps.io server, please see the following documentation: [Shiny Apps](https://shiny.rstudio.com/articles/shinyapps.html)
### Buttons

  - Extinction : Shuffle all columns to random values
  - Mutate : Shuffles a single value in a single column to a random value
  - Recombination : Shuffle a random set of contiguous columns to random values
  - Specific Mutation : Select column choice from drop-down, then click button to shuffle specific column to random value
  - Revert : Reverts player values to last set. Only preserves immediate last move, unalbe to revert mutliple times
  - Auto-Solve : Auto-solves current values utilizing random choices based on current 'adist()' from target values

## Authors

Rex Steele
Alvaro Pérez

## Acknowledgments

Inspiration, code snippets, etc.
* [Shiny from RStudio](https://shiny.rstudio.com/)
