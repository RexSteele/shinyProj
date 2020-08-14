library(shiny)
source("RandGen.R")

# Define UI for dataset viewer app ----
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

      # Input: Select entry for column to shuffle
      selectInput(inputId = "shuffleOneInput",
                   label = "Shuffle One Selection:",
                   choices = c("Column 1", "Column 2", "Column 3", "Column 4")),

      # Input: Button to shuffle column select by selectInput above
      actionButton(inputId = "shuffleOneButton",
                    label = "Shuffle One"),
      
      #radio select for locking
      checkboxGroupInput(inputId = "lockCheck", 
                         label = "Lock Column Selection",
                         c("Column 1" = "lockCol1",
                                     "Column 2" = "lockCol2",
                                     "Column 3" = "lockCol3",
                                     "Column 4" = "lockCol4")
                         )
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      
      # Outputs:
      fluidRow(
        column(2,
            textOutput("g1")
            ),
        column(2,
            textOutput("g2")
        ),
        column(2,
            textOutput("g3")
        ),
        column(2,
            textOutput("g4")
        ),
      ),
    
      fluidRow(
        column(2,
               textOutput("p1")
        ),
        column(2,
               textOutput("p2")
        ),
        column(2,
               textOutput("p3")
        ),
        column(2,
               textOutput("p4")
        ),
      ),
      #Cryptex Image
      img(src="cryptex2.png", align = "center", height = "300px", width = "500px")

    )
  )
)

goal <- reactiveValues(reactG1=createColumn(),reactG2=createColumn(),reactG3=createColumn(),reactG4=createColumn())
play <- reactiveValues(reactP1=createColumn(),reactP2=createColumn(),reactP3=createColumn(),reactP4=createColumn())

# Define server
server <- function(input, output) {
  
  #Sets up goal outputs, will change reactively
  output$g1 <- renderText({goal$reactG1})
  output$g2 <- renderText({goal$reactG2})
  output$g3 <- renderText({goal$reactG3})
  output$g4 <- renderText({goal$reactG4})
  #Sets up player outputs, will change reactively
  output$p1 <- renderText({play$reactP1})
  output$p2 <- renderText({play$reactP2})
  output$p3 <- renderText({play$reactP3})
  output$p4 <- renderText({play$reactP4})
  
  #Shuffle all button
  observeEvent(input$shuffleAll, {
    
    l1 <- "lockCol1" %in% isolate(input$lockCheck)
    l2 <- "lockCol2" %in% isolate(input$lockCheck)
    l3 <- "lockCol3" %in% isolate(input$lockCheck)
    l4 <- "lockCol4" %in% isolate(input$lockCheck)
    
    if(!l1){play$reactP1 <- createColumn()}
    if(!l2){play$reactP2 <- createColumn()}
    if(!l3){play$reactP3 <- createColumn()}
    if(!l4){play$reactP4 <- createColumn()}
  })
  
  #Shuffle one button
  observeEvent(input$shuffleOneButton, {
    if(input$shuffleOneInput == "Column 1"){
      play1 <- createColumn()
      output$p1 <- renderText({play1})
    }
    else if(input$shuffleOneInput == "Column 2"){
      play2 <- createColumn()
      output$p2 <- renderText({play2})
    }
    else if(input$shuffleOneInput == "Column 3"){
      play3 <- createColumn()
      output$p3 <- renderText({play3})
    }
    else{
      play4 <- createColumn()
      output$p4 <- renderText({play4})
    }
  })
}

shinyApp(ui = ui, server = server)
