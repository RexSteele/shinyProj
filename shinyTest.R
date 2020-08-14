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
                    label = "Shuffle One")
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
      img(src="cryptex2.png", alight = "center", height = "300px", width = "500px")

    )
  )
)

# Define server
server <- function(input, output) {
  
  #Sets up goal outputs, will change reactively
  output$g1 <- renderText({goal1})
  output$g2 <- renderText({goal2})
  output$g3 <- renderText({goal3})
  output$g4 <- renderText({goal4})
  #Sets up player outputs, will change reactively
  output$p1 <- renderText({play1})
  output$p2 <- renderText({play2})
  output$p3 <- renderText({play3})
  output$p4 <- renderText({play4})
  
  goal1 <- createColumn()
  goal2 <- createColumn()
  goal3 <- createColumn()
  goal4 <- createColumn()
  
  play1 <- createColumn()
  play2 <- createColumn()
  play3 <- createColumn()
  play4 <- createColumn()
  
  #Shuffle all button
  observeEvent(input$shuffleAll, {
    play1 <- createColumn()
    play2 <- createColumn()
    play3 <- createColumn()
    play4 <- createColumn()
    output$p1 <- renderText({play1})
    output$p2 <- renderText({play2})
    output$p3 <- renderText({play3})
    output$p4 <- renderText({play4})
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
  
  #Sets up goal outputs, will change reactively
  output$g1 <- renderText(goal1)
  output$g2 <- renderText(goal2)
  output$g3 <- renderText(goal3)
  output$g4 <- renderText(goal4)
  #Sets up player outputs, will change reactively
  output$p1 <- renderText(play1)
  output$p2 <- renderText(play2)
  output$p3 <- renderText(play3)
  output$p4 <- renderText(play4)
}

shinyApp(ui = ui, server = server)
