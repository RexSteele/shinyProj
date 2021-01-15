library(shiny)
library(shinyjs)
source("shinyDrop.R")


#Import data from RVD-DNA.txt as data frame
bindTab <- read.table(file = "RVD-DNA.txt", sep = "\t", header = TRUE, na.strings = '')
rownames(bindTab) <- bindTab[,1]
bindTab$X <- NULL

#df for player choices made
choices <- data.frame(matrix(ncol = 9, nrow = 1))
colnames(choices) <- c("p1","p2","p3","p4","p5","p6","p7","p8", "mt")


#USER INTERFACE
ui <- fluidPage(
  useShinyjs(),

  # App title ----
  titlePanel("Bio - Cryptex"),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

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
                  label = "Shuffle One Selection:",
                  choices = c("Column 1", "Column 2", "Column 3", "Column 4", "Column 5", "Column 6", "Column 7", "Column 8")),

      # Input: Button to shuffle column select by selectInput above
      actionButton(inputId = "shuffleOneButton",
                   label = "Shuffle One"),

      #radio select for locking
      checkboxGroupInput(inputId = "lockCheck",
                         label = "Lock Column Selection",
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
      htmlOutput("moveCount")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Outputs for binding sites
      fluidRow(
        column(8, align = "center",
               textOutput("b1")
        ),
      ),

      #Output for current sequence
      fluidRow(
        column(1,
               htmlOutput("p1")
        ),
        column(1,
               htmlOutput("p2")
        ),
        column(1,
               htmlOutput("p3")
        ),
        column(1,
               htmlOutput("p4")
        ),
        column(1,
               htmlOutput("p5")
        ),
        column(1,
               htmlOutput("p6")
        ),
        column(1,
               htmlOutput("p7")
        ),
        column(1,
               htmlOutput("p8")
        ),
      ),
      #Cryptex Image
      img(src="cryptex2.png", align = "center", height = "300px", width = "500px"),


      textOutput("aDist1"),
      textOutput("aDist2"),
      textOutput("aDist3"),
      textOutput("aDist4"),
      textOutput("aDist5")
    )
  )
)

#MODAL FUNCTION SECTION
#function for modal box to appear when win condition is reached
getEndModal <- function(secretPass){
  saveData(choices)
  choices <<- NULL
  choices <<- data.frame(matrix(ncol = 9, nrow = 1))
  colnames(choices) <<- c("p1","p2","p3","p4","p5","p6","p7","p8", "mt")
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

##SERVER
server <- function(input, output) {

  source("calcVals.R", local=TRUE)
  source("gameVals.R", local=TRUE)

  #update dataframe
  updateData <- function(){
    #add copy of sequence to choices dataframe
    nRow <- c(gsub("[^A-Z]", "", getCurrPlay1()), gsub("[^A-Z]", "", getCurrPlay2()), gsub("[^A-Z]", "", getCurrPlay3()), gsub("[^A-Z]", "", getCurrPlay4()), gsub("[^A-Z]", "", getCurrPlay5()), gsub("[^A-Z]", "", getCurrPlay6()), gsub("[^A-Z]", "", getCurrPlay7()), gsub("[^A-Z]", "", getCurrPlay8()), getMoveType())
    choices <<- rbind(choices, nRow)
  }

  #remove HTML tags from player values
  flushHTML <- function(){
    setCurrPlay1(gsub("[^A-Z]", "", getCurrPlay1()))
    setCurrPlay2(gsub("[^A-Z]", "", getCurrPlay2()))
    setCurrPlay3(gsub("[^A-Z]", "", getCurrPlay3()))
    setCurrPlay4(gsub("[^A-Z]", "", getCurrPlay4()))
    setCurrPlay5(gsub("[^A-Z]", "", getCurrPlay5()))
    setCurrPlay6(gsub("[^A-Z]", "", getCurrPlay6()))
    setCurrPlay7(gsub("[^A-Z]", "", getCurrPlay7()))
    setCurrPlay8(gsub("[^A-Z]", "", getCurrPlay8()))
  }

  #update player and bind values
  updateVals <- function(){
    output$p1 <- renderPrint({HTML(getCurrPlay1())})
    output$p2 <- renderPrint({HTML(getCurrPlay2())})
    output$p3 <- renderPrint({HTML(getCurrPlay3())})
    output$p4 <- renderPrint({HTML(getCurrPlay4())})
    output$p5 <- renderPrint({HTML(getCurrPlay5())})
    output$p6 <- renderPrint({HTML(getCurrPlay6())})
    output$p7 <- renderPrint({HTML(getCurrPlay7())})
    output$p8 <- renderPrint({HTML(getCurrPlay8())})
    output$b1 <- renderPrint({HTML(getWholeCurrBind())})
  }

  #updates adist values. Has to be inside server function due to manipulating output object
  adistOut <- function(){
    output$aDist1 <- renderText({paste("Secret Key 1 score: ", calcAdist(1))})
    if(!is.na(getGoalBind()[2])){
      output$aDist2 <- renderText({paste("Secret Key 2 score: ",calcAdist(2))})
    }
    if(!is.na(getGoalBind()[3])){
      output$aDist3 <- renderText({paste("Secret Key 3 score: ",calcAdist(3))})
    }
    if(!is.na(getGoalBind()[4])){
      output$aDist4 <- renderText({paste("Secret Key 4 score: ",calcAdist(4))})
    }
    if(!is.na(getGoalBind()[5])){
      output$aDist5 <- renderText({paste("Secret Key 5 score: ",calcAdist(5))})
    }
  }

  moveOut <- function(){
    output$moveCount <- renderPrint({HTML(paste0("<b>MOVE COUNT: ", getMoveCount(), "</b>"))})
  }

  #Launch Start Modal pop-up
  startModal()

  #SERVER EVENTS
  #Start game modal confirm button
  observeEvent(input$confirmKeys, {
    createGoal(input$keySelect)
    removeModal()

    #set player vals
    setCurrPlay1(createColumn())
    setCurrBind1(calcBindingSite(getCurrPlay1(), bindTab))
    setCurrPlay2(createColumn())
    setCurrBind2(calcBindingSite(getCurrPlay2(), bindTab))
    setCurrPlay3(createColumn())
    setCurrBind3(calcBindingSite(getCurrPlay3(), bindTab))
    setCurrPlay4(createColumn())
    setCurrBind4(calcBindingSite(getCurrPlay4(), bindTab))
    setCurrPlay5(createColumn())
    setCurrBind5(calcBindingSite(getCurrPlay5(), bindTab))
    setCurrPlay6(createColumn())
    setCurrBind6(calcBindingSite(getCurrPlay6(), bindTab))
    setCurrPlay7(createColumn())
    setCurrBind7(calcBindingSite(getCurrPlay7(), bindTab))
    setCurrPlay8(createColumn())
    setCurrBind8(calcBindingSite(getCurrPlay8(), bindTab))

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

    #Check which cols are locked, assigns boolean
    l1 <- "lockCol1" %in% input$lockCheck
    l2 <- "lockCol2" %in% input$lockCheck
    l3 <- "lockCol3" %in% input$lockCheck
    l4 <- "lockCol4" %in% input$lockCheck
    l5 <- "lockCol5" %in% input$lockCheck
    l6 <- "lockCol6" %in% input$lockCheck
    l7 <- "lockCol7" %in% input$lockCheck
    l8 <- "lockCol8" %in% input$lockCheck

    #If col is not locked, replace and re-do binding site eval
    if(!l1){
      setCurrPlay1(paste("<span style=\"color:red\">", createColumn(), "</span>"))
      setCurrBind1(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay1()), bindTab))
    }
    if(!l2){
      setCurrPlay2(paste("<span style=\"color:red\">", createColumn(), "</span>"))
      setCurrBind2(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay2()), bindTab))
    }
    if(!l3){
      setCurrPlay3(paste("<span style=\"color:red\">", createColumn(), "</span>"))
      setCurrBind3(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay3()), bindTab))
    }
    if(!l4){
      setCurrPlay4(paste("<span style=\"color:red\">", createColumn(), "</span>"))
      setCurrBind4(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay4()), bindTab))
    }
    if(!l5){
      setCurrPlay5(paste("<span style=\"color:red\">", createColumn(), "</span>"))
      setCurrBind5(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay5()), bindTab))
    }
    if(!l6){
      setCurrPlay6(paste("<span style=\"color:red\">", createColumn(), "</span>"))
      setCurrBind6(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay6()), bindTab))
    }
    if(!l7){
      setCurrPlay7(paste("<span style=\"color:red\">", createColumn(), "</span>"))
      setCurrBind7(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay7()), bindTab))
    }
    if(!l8){
      setCurrPlay8(paste("<span style=\"color:red\">", createColumn(), "</span>"))
      setCurrBind8(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay8()), bindTab))
    }

    ##Clean up
    #Output the values
    updateVals()
    setMoveType("Extinction")

    #Output adist values, increment moveCount, update dataframe
    addMoveCount()
    moveOut()
    adistOut()
    updateData()

    #Check win condition
    if(checkGoal() != FALSE){
      getEndModal(checkGoal())
    }
  })

  #Mutation button, shuffles random letter in random column
  observeEvent(input$mutation, {

    flushHTML()

    #save current player status
    setWholeOldPlay(getWholeCurrPlay())
    setWholeOldBind(getWholeCurrBind())

    #Pick random col, shuffle single char in that col
    randomCol <- round(runif(1,1,8))
    if(randomCol == 1){
      setCurrPlay1(oneRandomShuffle(getCurrPlay1()))
      setCurrBind1(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay1()), bindTab))
    }
    if(randomCol == 2){
      setCurrPlay2(oneRandomShuffle(getCurrPlay2()))
      setCurrBind2(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay2()), bindTab))
    }
    if(randomCol == 3){
      setCurrPlay3(oneRandomShuffle(getCurrPlay3()))
      setCurrBind3(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay3()), bindTab))
    }
    if(randomCol == 4){
      setCurrPlay4(oneRandomShuffle(getCurrPlay4()))
      setCurrBind4(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay4()), bindTab))
    }
    if(randomCol == 5){
      setCurrPlay5(oneRandomShuffle(getCurrPlay5()))
      setCurrBind5(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay5()), bindTab))
    }
    if(randomCol == 6){
      setCurrPlay6(oneRandomShuffle(getCurrPlay6()))
      setCurrBind6(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay6()), bindTab))
    }
    if(randomCol == 7){
      setCurrPlay7(oneRandomShuffle(getCurrPlay7()))
      setCurrBind7(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay7()), bindTab))
    }
    if(randomCol == 8){
      setCurrPlay8(oneRandomShuffle(getCurrPlay8()))
      setCurrBind8(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay8()), bindTab))
    }

    ##Clean up
    #Output the values
    updateVals()
    setMoveType("Mutation")

    #Output adist values, increment moveCount, update dataframe
    addMoveCount()
    moveOut()
    adistOut()
    updateData()

    #Check win condition
    if(checkGoal() != FALSE){
      getEndModal(checkGoal())
    }
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

    #save reactives as list to new var so it can be iterated
    recombVar <- c(getCurrPlay1(), getCurrPlay2(), getCurrPlay3(), getCurrPlay4(), getCurrPlay5(), getCurrPlay6(), getCurrPlay7(), getCurrPlay8())

    #iterate new list an modify as appropriate
    for(i in startCol:endCol){
      recombVar[i] <- paste("<span style=\"color:red\">", createColumn(), "</span>", sep= "", collapse = "")
      if(i==1){setCurrPlay1(recombVar[i])}
      if(i==2){setCurrPlay2(recombVar[i])}
      if(i==3){setCurrPlay3(recombVar[i])}
      if(i==4){setCurrPlay4(recombVar[i])}
      if(i==5){setCurrPlay5(recombVar[i])}
      if(i==6){setCurrPlay6(recombVar[i])}
      if(i==7){setCurrPlay7(recombVar[i])}
      if(i==8){setCurrPlay8(recombVar[i])}
    }

    #re-assign back to vars
    # setWholeCurrPlay(paste(recombVar, sep = "", collapse = ""))
    setCurrBind1(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay1()), bindTab))
    setCurrBind2(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay2()), bindTab))
    setCurrBind3(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay3()), bindTab))
    setCurrBind4(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay4()), bindTab))
    setCurrBind5(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay5()), bindTab))
    setCurrBind6(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay6()), bindTab))
    setCurrBind7(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay7()), bindTab))
    setCurrBind8(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay8()), bindTab))


    ##Clean up
    #Output the values
    updateVals()
    setMoveType("Recombination")

    #Output adist values, increment moveCount, update dataframe
    addMoveCount()
    moveOut()
    adistOut()
    updateData()

    #Check win condition
    if(checkGoal() != FALSE){
      getEndModal(checkGoal())
    }
  })

  #Shuffle one button
  observeEvent(input$shuffleOneButton, {

    flushHTML()

    #save current player status
    setWholeOldPlay(getWholeCurrPlay())
    setWholeOldBind(getWholeCurrBind())

    #Shuffle column based on input
    if(input$shuffleOneInput == "Column 1"){
      setCurrPlay1(paste("<span style=\"color:red\">", createColumn(), "</span>"))
      setCurrBind1(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay1()), bindTab))
    }
    if(input$shuffleOneInput == "Column 2"){
      setCurrPlay2(paste("<span style=\"color:red\">", createColumn(), "</span>"))
      setCurrBind2(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay2()), bindTab))
    }
    if(input$shuffleOneInput == "Column 3"){
      setCurrPlay3(paste("<span style=\"color:red\">", createColumn(), "</span>"))
      setCurrBind3(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay3()), bindTab))
    }
    if(input$shuffleOneInput == "Column 4"){
      setCurrPlay4(paste("<span style=\"color:red\">", createColumn(), "</span>"))
      setCurrBind4(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay4()), bindTab))
    }
    if(input$shuffleOneInput == "Column 5"){
      setCurrPlay5(paste("<span style=\"color:red\">", createColumn(), "</span>"))
      setCurrBind5(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay5()), bindTab))
    }
    if(input$shuffleOneInput == "Column 6"){
      setCurrPlay6(paste("<span style=\"color:red\">", createColumn(), "</span>"))
      setCurrBind6(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay6()), bindTab))
    }
    if(input$shuffleOneInput == "Column 7"){
      setCurrPlay7(paste("<span style=\"color:red\">", createColumn(), "</span>"))
      setCurrBind7(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay7()), bindTab))
    }
    if(input$shuffleOneInput == "Column 8"){
      setCurrPlay8(paste("<span style=\"color:red\">", createColumn(), "</span>"))
      setCurrBind8(calcBindingSite(gsub("[^A-Z]", "", getCurrPlay8()), bindTab))
    }

    ##Clean up
    #Output the values
    updateVals()
    setMoveType("Shuffle One")

    #Output adist values, increment moveCount, update dataframe
    addMoveCount()
    moveOut()
    adistOut()
    updateData()

    #Check win condition
    if(checkGoal() != FALSE){
      getEndModal(checkGoal())
    }
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
    while(checkGoal() == FALSE){
      if(calcAdist(1) >= 5){
        shinyjs::click("shuffleAll")
        invalidateLater(3000)
        Sys.sleep(2)
        print("shuffles")
      }else{
        shinyjs::click("recomb")
        invalidateLater(3000)
        Sys.sleep(2)
        print("recomb")
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
