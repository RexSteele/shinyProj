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
                   choices = c("Column 1", "Column 2", "Column 3", "Column 4", "Column 5", "Column 6", "Column 7", "Column 8")),

      # Input: Button to shuffle column select by selectInput above
      actionButton(inputId = "shuffleOneButton",
                    label = "Shuffle One")
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output:
      verbatimTextOutput("Yours"),
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
      
      #Cryptex Image
      img(src="cryptex2.png", alight = "center", height = "300px", width = "500px")

    )
  )
)

# Define server
server <- function(input, output) {
  
  goal1 <- createColumn()
  goal2 <- createColumn()
  goal3 <- createColumn()
  goal4 <- createColumn()
  
  play1 <- createColumn()
  play2 <- createColumn()
  play3 <- createColumn()
  play4 <- createColumn()
  
  output$g1 <- renderText({
    createColumn()
  })
  output$g2 <- renderText({
    createColumn()
  })
  output$g3 <- renderText({
    createColumn()
  })
  output$g4 <- renderText({
    createColumn()
  })
}

shinyApp(ui = ui, server = server)
