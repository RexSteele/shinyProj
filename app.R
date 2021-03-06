    library(shiny)
library(shinyBS)
library(tidyverse)
source("shinyDrop.R")


#Import data from RVD-DNA.txt as data frame
bindTab <- read.table(file = "RVD-DNA.txt", sep = "\t", header = TRUE, na.strings = '')
rownames(bindTab) <- bindTab[,1]
bindTab$X <- NULL

#df for player choices made
choices <- data.frame(matrix(ncol = 14, nrow = 1))
colnames(choices) <- c("p1","p2","p3","p4","p5","p6","p7","p8", "mt", "adist1", "adist2", "adist3", "adist4", "adist5")


#USER INTERFACE
ui <- fluidPage(
  shinyBS:::shinyBSDep,

  # App title ----
  titlePanel("Bio - Cryptex"),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      width = 4,

      # Input: Button to shuffle all values
      actionButton(inputId = "shuffleAll",
                   label = "Extinction"),

      # Input: Button to shuffle one random letter in random column
      actionButton(inputId = "mutation",
                   label = "Mutate"),

      # Input: Button to shuffle a random set of contiguous columns
      actionButton(inputId = "recomb",
                   label = "Recombination"),

      # Input: Select entry for column to shuffle
      selectInput(inputId = "shuffleOneInput",
                  label = "Choose specific column to mutate:",
                  choices = c("Column 1", "Column 2", "Column 3", "Column 4", "Column 5", "Column 6", "Column 7", "Column 8")),

      # Input: Button to shuffle column select by selectInput above
      actionButton(inputId = "shuffleOneButton",
                   label = "Specific mutation"),

      #radio select for locking
      checkboxGroupInput(inputId = "lockCheck",
                         label = "Lock Column before extinction:",
                         c("Column 1" = "lockCol1",
                           "Column 2" = "lockCol2",
                           "Column 3" = "lockCol3",
                           "Column 4" = "lockCol4",
                           "Column 5" = "lockCol5",
                           "Column 6" = "lockCol6",
                           "Column 7" = "lockCol7",
                           "Column 8" = "lockCol8")
      ),

      #Revert to prior combination, can only go one step back
      actionButton(inputId = "revert",
                   label = "Revert"),

      #Auto-solve button
      actionButton(inputId = "autoSolve",
                   label = "Auto-Solve"),

      #moveCount output
      htmlOutput("moveCount"),

      #author tag
      htmlOutput("author", style="color:#888; font-size: 12px;")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Outputs for binding sites
      fluidRow(
        column(9, align = "center", "Binding Site", style = "font-size:40px; text-shadow: 3px 3px 3px #aaa;",
               textOutput("b1")
        )
      ),

      #Output for current sequence
      fluidRow(style = "background-image: url(Criptex_v2_copy.png); width: 600px; height: 185px; margin-top: 50px; font-weight: bold;",
         htmlOutput("p1", style="margin-left: 90px; margin-top: 85px"),
         htmlOutput("p2", style="margin-left: 155px; margin-top: -20px"),
         htmlOutput("p3", style="margin-left: 225px; margin-top: -23px"),
         htmlOutput("p4", style="margin-left: 290px; margin-top: -20px"),
         htmlOutput("p5", style="margin-left: 360px; margin-top: -20px"),
         htmlOutput("p6", style="margin-left: 425px; margin-top: -20px"),
         htmlOutput("p7", style="margin-left: 495px; margin-top: -20px"),
         htmlOutput("p8", style="margin-left: 560px; margin-top: -20px")
      ),

      fluidRow(style = "font-size:40px; text-shadow: 3px 3px 3px #aaa;",
        textOutput("aDist1"),
        textOutput("aDist2"),
        textOutput("aDist3"),
        textOutput("aDist4"),
        textOutput("aDist5")
      ),
    )
  )
)

#MODAL FUNCTION SECTION
#function for modal box to appear when win condition is reached
getEndModal <- function(secretPass){
  saveData(choices)
  choices <<- NULL
  choices <<- data.frame(matrix(ncol = 14, nrow = 1))
  colnames(choices) <<- c("p1","p2","p3","p4","p5","p6","p7","p8", "mt", "adist1", "adist2", "adist3", "adist4", "adist5")
  showModal(modalDialog(
    title = "You found the secret message ", secretPass, "!",
    "Congratulations, great job!",
    actionButton(inputId = "playAgain",
                  label = "Play Again"),
    actionButton(inputId = "quit",
                  label = "Quit"),
    easyClose = TRUE,
    footer = NULL
  ))
}

#function for modal box to appear at start, prompt for number of secret keys
startModal <- function(){
  showModal(modalDialog(
    title = "Bio-Cryptex",
    selectInput(inputId = "keySelect",
                label = "Select number of secret keys, then hit 'Confirm' to start the game!",
                choices = c("1", "2", "3", "4", "5")),
    actionButton(inputId = "confirmKeys",
                 label = "Confirm"),
    easyClose = FALSE,
    footer = NULL
  ))
}

#Modal for autosolve button, provides options for multiple auto solve techniques
autoSolveModal <- function(){
  showModal(modalDialog(
    title = "Auto-Solve",
    actionButton(inputId = "as1",
                 label = "Extinction >= 5, Recombination < 5"),
    actionButton(inputId = "as2",
                 label = "Extinction > 5, Mutation <= 5"),
    actionButton(inputId = "as3",
                 label = "Extinction > 5, Recombination > 2, Mutation <= 2"),
    easyClose = TRUE,
    footer = NULL
  ))
}

##SERVER
server <- function(input, output, session) {

  source("calcVals.R", local=TRUE)
  source("gameVals.R", local=TRUE)

  output$author <- renderPrint({HTML("Developed by Rex Steele and Alvaro L Perez-Quintero (Colorado State University, Agricultural Biology)")})
  addTooltip(session, id = 'shuffleAll', title = "Shuffles all columns",
             placement = "bottom", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = 'mutation', title = "Shuffles single value in one column",
             placement = "bottom", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = 'recomb', title = "Shuffles random set of contiguous columns",
             placement = "bottom", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = 'shuffleOneInput', title = "Select column to mutate",
             placement = "bottom", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = 'shuffleOneButton', title = "Shuffles column chosen by dropdown list",
             placement = "bottom", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = 'revert', title = "Reverts once to prior column set",
             placement = "bottom", trigger = "hover", options = list(delay = list(show=1000)))
  addTooltip(session, id = 'autoSolve', title = "Automatically solves current puzzle for Secret Key 1",
             placement = "bottom", trigger = "hover", options = list(delay = list(show=1000)))

  #update dataframe
  updateData <- function(){
    #add copy of sequence to choices dataframe
    nRow <- c(gsub("[^A-Z]", "", getCurrPlay(1)), gsub("[^A-Z]", "", getCurrPlay(2)), gsub("[^A-Z]", "", getCurrPlay(3)), gsub("[^A-Z]", "", getCurrPlay(4)), gsub("[^A-Z]", "", getCurrPlay(5)), gsub("[^A-Z]", "", getCurrPlay(6)), gsub("[^A-Z]", "", getCurrPlay(7)), gsub("[^A-Z]", "", getCurrPlay(8)), getMoveType(), calcAdist(1), calcAdist(2), calcAdist(3), calcAdist(4), calcAdist(5))
    choices <<- rbind(choices, nRow)
  }

  #remove HTML tags from player values
  flushHTML <- function(){
    for(x in 1:8){
      setCurrPlay(gsub("[^A-Z]", "", getCurrPlay(x)), x)
    }
  }

  #update player and bind values
  updateVals <- function(){
    output$p1 <- renderPrint({HTML(getCurrPlay(1))})
    output$p2 <- renderPrint({HTML(getCurrPlay(2))})
    output$p3 <- renderPrint({HTML(getCurrPlay(3))})
    output$p4 <- renderPrint({HTML(getCurrPlay(4))})
    output$p5 <- renderPrint({HTML(getCurrPlay(5))})
    output$p6 <- renderPrint({HTML(getCurrPlay(6))})
    output$p7 <- renderPrint({HTML(getCurrPlay(7))})
    output$p8 <- renderPrint({HTML(getCurrPlay(8))})
    output$b1 <- renderPrint({HTML(gsub("([A-Z])", "\\1 \\2", getWholeCurrBind()))})
  }

  #updates adist values. Has to be inside server function due to manipulating output object
  adistOut <- function(){
    output$aDist1 <- renderText({paste("Secret Key 1 score: ", calcAdist(1))})
    if(!is.na(getGoalArr()[2])){
      output$aDist2 <- renderText({paste("Secret Key 2 score: ",calcAdist(2))})
    }
    if(!is.na(getGoalArr()[3])){
      output$aDist3 <- renderText({paste("Secret Key 3 score: ",calcAdist(3))})
    }
    if(!is.na(getGoalArr()[4])){
      output$aDist4 <- renderText({paste("Secret Key 4 score: ",calcAdist(4))})
    }
    if(!is.na(getGoalArr()[5])){
      output$aDist5 <- renderText({paste("Secret Key 5 score: ",calcAdist(5))})
    }
  }

  moveOut <- function(){
    output$moveCount <- renderPrint({HTML(paste0("<b>MOVE COUNT: ", getMoveCount(), "</b>"))})
  }

  cleanUp <- function(curMove){
    ##Clean up
    #Output the values
    updateVals()
    setMoveType(curMove)

    #Output adist values, increment moveCount, update dataframe
    addMoveCount()
    moveOut()
    adistOut()
    updateData()

    #Check win condition
    if(checkGoal() != FALSE){
      getEndModal(checkGoal())
    }
  }

  #Launch Start Modal pop-up
  startModal()

  #SERVER EVENTS
  #Start game modal confirm button
  observeEvent(input$confirmKeys, {
    createGoal(input$keySelect)
    removeModal()

    #set player vals
    for(x in 1:8){
      setCurrPlay(createColumn(), x)
      setCurrBind(calcBindingSite(getCurrPlay(x), bindTab), x)
    }

    #set old vals to current vals for first iteration
    setWholeOldPlay(getWholeCurrPlay())
    setWholeOldBind(getWholeCurrBind())

    #Output the values
    updateVals()

    #Reset then output adist values
    output$aDist1 <- renderText({""})
    output$aDist2 <- renderText({""})
    output$aDist3 <- renderText({""})
    output$aDist4 <- renderText({""})
    output$aDist5 <- renderText({""})
    adistOut()

    #Start move Count
    moveOut()

    #Check win condition
    if(checkGoal() != FALSE){
      getEndModal(checkGoal())
    }
  })

  #Shuffle all button
  observeEvent(input$shuffleAll, {

    flushHTML()

    #save current player status
    setWholeOldPlay(getWholeCurrPlay())
    setWholeOldBind(getWholeCurrBind())

    #If col is not locked, replace and re-do binding site eval
    for(x in 1:8){
      if(!(paste("lockCol", toString(x), sep="") %in% input$lockCheck)){
        setCurrPlay(paste("<span style=\"color:red\">", createColumn(), "</span>"), x)
        setCurrBind(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay(x)), bindTab), x)
      }
    }

    cleanUp("Extinction")
  })

  #Mutation button, shuffles random letter in random column
  observeEvent(input$mutation, {

    flushHTML()

    #save current player status
    setWholeOldPlay(getWholeCurrPlay())
    setWholeOldBind(getWholeCurrBind())

    #Pick random col, shuffle single char in that col
    randomCol <- round(runif(1,1,8))
    setCurrPlay(oneRandomShuffle(getCurrPlay(randomCol)), randomCol)
    setCurrBind(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay(randomCol)), bindTab), randomCol)

    cleanUp("Mutation")
  })

  #Recombination button, shuffles random number of contiguous columns
  observeEvent(input$recomb, {

    flushHTML()

    #save current player status
    setWholeOldPlay(getWholeCurrPlay())
    setWholeOldBind(getWholeCurrBind())

    #Pick random number of cols, then choose acceptable start col for contiguous column selection
    colAmount <- round(runif(1,1,8))
    startCol <- round(runif(1,1,9-colAmount))
    endCol <- startCol + colAmount - 1
    markCol <- startCol

    for(x in startCol:endCol){
      setCurrPlay(paste("<span style=\"color:red\">", createColumn(), "</span>", sep= "", collapse = ""), x)
      setCurrBind(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay(x)), bindTab), x)
    }

    cleanUp("Recombination")
  })

  #Shuffle one button
  observeEvent(input$shuffleOneButton, {

    flushHTML()

    #save current player status
    setWholeOldPlay(getWholeCurrPlay())
    setWholeOldBind(getWholeCurrBind())

    #Shuffle column based on input
    for(x in 1:8){
      if(paste("Column", toString(x), sep=" ") %in% input$shuffleOneInput){
        setCurrPlay(paste("<span style=\"color:red\">", createColumn(), "</span>"), x)
        setCurrBind(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay(x)), bindTab), x)
      }
    }

    cleanUp("Shuffle One")
  })

  #Revert button, reverts to set from immediately prior to last action
  observeEvent(input$revert, {

    #Reset values
    setWholeCurrPlay(getWholeOldPlay())
    setWholeCurrBind(getWholeOldBind())

    #add marker row to show prior row was reverted
    nRow <- c("^^", "^^", "^^", "^^", "^^", "^^", "^^", "^^", "Reverted")
    choices <<- rbind(choices, nRow)

    #Output the values
    updateVals()

    #Output adist values
    subMoveCount()
    moveOut()
    adistOut()
  })

  observeEvent(input$autoSolve, {
    autoSolveModal()
  })

  #Auto Solve functionality: Utilizes same functionality as standard gameplay buttons do above. Unable to call directly,
      # thus the reuse of the code. Should be refactored to functionalize gameplay features (Extinction, Recomb, etc),
      # then call those functions as needed for both the buttons above and the auto solve functionality below
  ##AS1
  observeEvent(input$as1, {
    while(checkGoal() == FALSE){

      flushHTML()
      tempADIST = calcAdist(1)

      #save current player status
      setWholeOldPlay(getWholeCurrPlay())
      setWholeOldBind(getWholeCurrBind())

      if(calcAdist(1) >= 5){
        #Extinction code
        for(x in 1:8){
          if(!(paste("lockCol", toString(x), sep="") %in% input$lockCheck)){
            setCurrPlay(paste("<span style=\"color:red\">", createColumn(), "</span>"), x)
            setCurrBind(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay(x)), bindTab), x)
          }
        }

        cleanUp("Extinction")

      }else{
        #Recombination code
        colAmount <- round(runif(1,1,8))
        startCol <- round(runif(1,1,9-colAmount))
        endCol <- startCol + colAmount - 1
        markCol <- startCol

        for(x in startCol:endCol){
          setCurrPlay(paste("<span style=\"color:red\">", createColumn(), "</span>", sep= "", collapse = ""), x)
          setCurrBind(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay(x)), bindTab), x)
        }

        cleanUp("Recombination")
      }

      if(tempADIST < calcAdist(1)){
        #Reset values
        setWholeCurrPlay(getWholeOldPlay())
        setWholeCurrBind(getWholeOldBind())

        #add marker row to show prior row was reverted
        nRow <- c("^^", "^^", "^^", "^^", "^^", "^^", "^^", "^^", "Reverted")
        choices <<- rbind(choices, nRow)

        #Output the values
        updateVals()

        #Output adist values
        subMoveCount()
        moveOut()
        adistOut()
      }
    }
})

    ##AS2
    observeEvent(input$as2, {
      while(checkGoal() == FALSE){

        flushHTML()
        tempADIST = calcAdist(1)

        #save current player status
        setWholeOldPlay(getWholeCurrPlay())
        setWholeOldBind(getWholeCurrBind())

        if(calcAdist(1) > 5){
          #If col is not locked, replace and re-do binding site eval
          for(x in 1:8){
            if(!(paste("lockCol", toString(x), sep="") %in% input$lockCheck)){
              setCurrPlay(paste("<span style=\"color:red\">", createColumn(), "</span>"), x)
              setCurrBind(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay(x)), bindTab), x)
            }
          }

          cleanUp("Extinction")

        }else{
          #Mutate code
          randomCol <- round(runif(1,1,8))
          setCurrPlay(oneRandomShuffle(getCurrPlay(randomCol)), randomCol)
          setCurrBind(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay(randomCol)), bindTab), randomCol)

          cleanUp("Mutation")
        }

        if(tempADIST < calcAdist(1)){
          #Reset values
          setWholeCurrPlay(getWholeOldPlay())
          setWholeCurrBind(getWholeOldBind())

          #add marker row to show prior row was reverted
          nRow <- c("^^", "^^", "^^", "^^", "^^", "^^", "^^", "^^", "Reverted")
          choices <<- rbind(choices, nRow)

          #Output the values
          updateVals()

          #Output adist values
          subMoveCount()
          moveOut()
          adistOut()
        }
      }
})

      ##AS3
      observeEvent(input$as3, {
        while(checkGoal() == FALSE){

          flushHTML()
          tempADIST = calcAdist(1)

          #save current player status
          setWholeOldPlay(getWholeCurrPlay())
          setWholeOldBind(getWholeCurrBind())

          if(calcAdist(1) > 5){
            #Extinction code
            for(x in 1:8){
              if(!(paste("lockCol", toString(x), sep="") %in% input$lockCheck)){
                setCurrPlay(paste("<span style=\"color:red\">", createColumn(), "</span>"), x)
                setCurrBind(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay(x)), bindTab), x)
              }
            }

            cleanUp("Extinction")

          }else if(calcAdist(1) > 2){
            #Recomb code
            colAmount <- round(runif(1,1,8))
            startCol <- round(runif(1,1,9-colAmount))
            endCol <- startCol + colAmount - 1
            markCol <- startCol

            for(x in startCol:endCol){
              setCurrPlay(paste("<span style=\"color:red\">", createColumn(), "</span>", sep= "", collapse = ""), x)
              setCurrBind(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay(x)), bindTab), x)
            }
            cleanUp("Recombination")

          }else{
            #Mutate code
            randomCol <- round(runif(1,1,8))
            setCurrPlay(oneRandomShuffle(getCurrPlay(randomCol)), randomCol)
            setCurrBind(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay(randomCol)), bindTab), randomCol)

            cleanUp("Mutation")
          }

          if(tempADIST < calcAdist(1)){
            #Reset values
            setWholeCurrPlay(getWholeOldPlay())
            setWholeCurrBind(getWholeOldBind())

            #add marker row to show prior row was reverted
            nRow <- c("^^", "^^", "^^", "^^", "^^", "^^", "^^", "^^", "Reverted")
            choices <<- rbind(choices, nRow)

            #Output the values
            updateVals()

            #Output adist values
            subMoveCount()
            moveOut()
            adistOut()
          }
        }
  })

  #Play again button on win modal
  observeEvent(input$playAgain, {
    removeModal()
    startModal()
  })

  #Quit button on win modal
  observeEvent(input$quit, {
    stopApp(returnValue = invisible())
  })
}

#Assigns UI and Server values to App
shinyApp(ui = ui, server = server)
