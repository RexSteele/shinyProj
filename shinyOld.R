library(shiny)
source("RandGen.R")

#Standard string for revert event
oldPlay <- ""

#Import data from RVD-DNA.txt as data frame
bondTab <- read.table(file = "RVD-DNA.txt", sep = "\t", header = TRUE, na.strings = '')
rownames(bondTab) <- bondTab[,1]
bondTab$X <- NULL

#df for player choices made
choices <- data.frame(matrix(ncol = 8, nrow = 1))
colnames(choices) <- c("p1","p2","p3","p4","p5","p6","p7","p8")


#USER INTERFACE
ui <- fluidPage(
  
  # App title ----
  titlePanel("Bio - Cryptex"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Button to shuffle all values
      actionButton(inputId = "shuffleAll",
                   label = "Shuffle All"),
      
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
                   label = "Revert")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Outputs for bonding sites
      fluidRow(
        column(8, align = "center",
               textOutput("b1")
        ),
      ),
      
      #Output for current sequence
      fluidRow(
        column(1,
               textOutput("p1")
        ),
        column(1,
               textOutput("p2")
        ),
        column(1,
               textOutput("p3")
        ),
        column(1,
               textOutput("p4")
        ),
        column(1,
               textOutput("p5")
        ),
        column(1,
               textOutput("p6")
        ),
        column(1,
               textOutput("p7")
        ),
        column(1,
               textOutput("p8")
        ),
      ),
      #Cryptex Image
      img(src="cryptex2.png", align = "center", height = "300px", width = "500px"),
      
      
      textOutput("aDist")
    )
  )
)



##HELPER FUNCTION SECTION
#function for modal box to appear when win condition is reached
getModal <- function(){
  showModal(modalDialog(
    title = "You found the secret message!",
    "Congratulations, great job!",
    easyClose = TRUE,
    footer = NULL
  ))
}

startModal <- function(){
  showModal(modalDialog(
    title = "Bio-Cryptex",
    selectInput(inputId = "keySelect",
                label = "Select number of secret keys:",
                choices = c("1", "2", "3", "4", "5", "6", "7", "8")),
    actionButton(inputId = "confirmKeys",
                 label = "Confirm"),
    easyClose = FALSE,
    footer = NULL
  ))
}


##GLOBAL REACTIVE
#Initial values. Set to reactive so if changed, updates output
play <- reactiveValues(
  reactP1=createColumn(),reactP2=createColumn(),reactP3=createColumn(),reactP4=createColumn(),reactP5=createColumn(),reactP6=createColumn(),reactP7=createColumn(),reactP8=createColumn())


##SERVER
server <- function(input, output) {
  
  showModal(startModal)
  
  #Sets up player outputs, will change reactively
  output$p1 <- renderText({play$reactP1})
  output$p2 <- renderText({play$reactP2})
  output$p3 <- renderText({play$reactP3})
  output$p4 <- renderText({play$reactP4})
  output$p5 <- renderText({play$reactP5})
  output$p6 <- renderText({play$reactP6})
  output$p7 <- renderText({play$reactP7})
  output$p8 <- renderText({play$reactP8})
  
  #Sets up bonding site outputs, will change reactively
  output$b1 <- renderText({getBondingSite(play$reactP1, play$reactP2, play$reactP3, play$reactP4, play$reactP5, play$reactP6, play$reactP7, play$reactP8, bondTab)})

  #Shuffle all button
  observeEvent(input$shuffleAll, {
    
    #save current player status
    oldPlay <<- paste(play$reactP1, play$reactP2, play$reactP3, play$reactP4, play$reactP5, play$reactP6, play$reactP7, play$reactP8, sep="", collapse="")
    saveBond()
    
    #Check which cols are locked, assigns boolean
    l1 <- "lockCol1" %in% input$lockCheck
    l2 <- "lockCol2" %in% input$lockCheck
    l3 <- "lockCol3" %in% input$lockCheck
    l4 <- "lockCol4" %in% input$lockCheck
    l5 <- "lockCol5" %in% input$lockCheck
    l6 <- "lockCol6" %in% input$lockCheck
    l7 <- "lockCol7" %in% input$lockCheck
    l8 <- "lockCol8" %in% input$lockCheck
    
    #If col is not locked, replace and re-do bonding site eval
    if(!l1){play$reactP1 <- createColumn()}
    if(!l2){play$reactP2 <- createColumn()}
    if(!l3){play$reactP3 <- createColumn()}
    if(!l4){play$reactP4 <- createColumn()}
    if(!l5){play$reactP5 <- createColumn()}
    if(!l6){play$reactP6 <- createColumn()}
    if(!l7){play$reactP7 <- createColumn()}
    if(!l8){play$reactP8 <- createColumn()}
    
    ##Clean up
    #add copy of sequence to choices dataframe
    nRow <- c(play$reactP1, play$reactP2, play$reactP3, play$reactP4, play$reactP5, play$reactP6, play$reactP7, play$reactP8)
    choices <<- rbind(choices, nRow)
    
    #Change adist
    output$aDist <- renderText({getAdist()})
    
    #Check win condition
    if(checkGoal(oldPlay)){
      getModal(oldPlay)
    }
  })
  
  #Mutation button, shuffles random letter in random column
  observeEvent(input$mutation, {
    
    #save current player status to standard string
    oldPlay <<- paste(play$reactP1, play$reactP2, play$reactP3, play$reactP4, play$reactP5, play$reactP6, play$reactP7, play$reactP8, sep="", collapse="")
    saveBond()
    
    #Pick random col, shuffle single char in that col
    randomCol <- round(runif(1,1,8))
    if(randomCol == 1){play$reactP1 <- oneRandomShuffle(play$reactP1)}
    if(randomCol == 2){play$reactP2 <- oneRandomShuffle(play$reactP2)}
    if(randomCol == 3){play$reactP3 <- oneRandomShuffle(play$reactP3)}
    if(randomCol == 4){play$reactP4 <- oneRandomShuffle(play$reactP4)}
    if(randomCol == 5){play$reactP5 <- oneRandomShuffle(play$reactP5)}
    if(randomCol == 6){play$reactP6 <- oneRandomShuffle(play$reactP6)}
    if(randomCol == 7){play$reactP7 <- oneRandomShuffle(play$reactP7)}
    if(randomCol == 8){play$reactP8 <- oneRandomShuffle(play$reactP8)}
    
    #Clean up
    #add copy of sequence to choices dataframe
    nRow <- c(play$reactP1, play$reactP2, play$reactP3, play$reactP4, play$reactP5, play$reactP6, play$reactP7, play$reactP8)
    choices <<- rbind(choices, nRow)
    
    #Change adist
    output$aDist <- renderText({getAdist()})
    
    #Check win condition
    if(checkGoal(oldPlay)){
      getModal(oldPlay)
    }
  })
  
  #Recombination button, shuffles random number of contiguous columns
  observeEvent(input$recomb, {
    
    #save current player status to standard string
    oldPlay <<- paste(play$reactP1, play$reactP2, play$reactP3, play$reactP4, play$reactP5, play$reactP6, play$reactP7, play$reactP8, sep="", collapse="")
    saveBond()
    
    #Pick random number of cols, then choose acceptable start col for contiguous column selection
    colAmount <- round(runif(1,1,8))
    startCol <- round(runif(1,1,9-colAmount))
    endCol <- startCol + colAmount - 1
    markCol <- startCol
    
    #save reactives as list to new far so it can be iterated
    recombVar <- c(play$reactP1, play$reactP2, play$reactP3, play$reactP4, play$reactP5, play$reactP6, play$reactP7, play$reactP8)
    
    #iterate new list an modify as appropriate
    for(i in startCol:endCol){
      recombVar[markCol] <- createColumn()
      markCol <- markCol + 1
    }
    
    #re-assign basck to reactive var
    play$reactP1 <- recombVar[1]
    play$reactP2 <- recombVar[2]
    play$reactP3 <- recombVar[3]
    play$reactP4 <- recombVar[4]
    play$reactP5 <- recombVar[5]
    play$reactP6 <- recombVar[6]
    play$reactP7 <- recombVar[7]
    play$reactP8 <- recombVar[8]
    
    #Clean up
    #add copy of sequence to choices dataframe
    nRow <- c(play$reactP1, play$reactP2, play$reactP3, play$reactP4, play$reactP5, play$reactP6, play$reactP7, play$reactP8)
    choices <<- rbind(choices, nRow)
    
    #Change adist
    output$aDist <- renderText({getAdist()})
    
    #Check win condition
    if(checkGoal(oldPlay)){
      getModal(oldPlay)
    }
  })
  
  #Shuffle one button
  observeEvent(input$shuffleOneButton, {
    
    #save current player status to standard string
    oldPlay <<- paste(play$reactP1, play$reactP2, play$reactP3, play$reactP4, play$reactP5, play$reactP6, play$reactP7, play$reactP8, sep="", collapse="")
    saveBond()
    
    #Shuffle column based on input
    if(input$shuffleOneInput == "Column 1"){
      play$reactP1 <- createColumn()
    }
    if(input$shuffleOneInput == "Column 2"){
      play$reactP2 <- createColumn()
    }
    if(input$shuffleOneInput == "Column 3"){
      play$reactP3 <- createColumn()
    }
    if(input$shuffleOneInput == "Column 4"){
      play$reactP4 <- createColumn()
    }
    if(input$shuffleOneInput == "Column 5"){
      play$reactP1 <- createColumn()
    }
    if(input$shuffleOneInput == "Column 6"){
      play$reactP2 <- createColumn()
    }
    if(input$shuffleOneInput == "Column 7"){
      play$reactP3 <- createColumn()
    }
    if(input$shuffleOneInput == "Column 8"){
      play$reactP4 <- createColumn()
    }
    
    #Clean up
    #add copy of sequence to choices dataframe
    nRow <- c(play$reactP1, play$reactP2, play$reactP3, play$reactP4, play$reactP5, play$reactP6, play$reactP7, play$reactP8)
    choices <<- rbind(choices, nRow)
    
    #Change adist
    output$aDist <- renderText({getAdist()})
    
    #Check win condition
    if(checkGoal(oldPlay)){
      getModal(oldPlay)
    }
  })
  
  #Revert button, reverts to set from immediately prior to last action
  observeEvent(input$revert, {
    
    #Reset values
    play$reactP1 <- substr(oldPlay, 1, 2)
    play$reactP2 <- substr(oldPlay, 3, 4)
    play$reactP3 <- substr(oldPlay, 5, 6)
    play$reactP4 <- substr(oldPlay, 7, 8)
    play$reactP5 <- substr(oldPlay, 9, 10)
    play$reactP6 <- substr(oldPlay, 11, 12)
    play$reactP7 <- substr(oldPlay, 13, 14)
    play$reactP8 <- substr(oldPlay, 15, 16)
    
    output$b1 <- renderText(getOldBond())
    
    #add marker row to show prior row was reverted
    nRow <- c("^^", "^^", "RE", "VE", "RT", "ED", "^^", "^^")
    choices <<- rbind(choices, nRow)
    
    #Change adist
    output$aDist <- renderText({getAdist()})
  })
}

shinyApp(ui = ui, server = server)
