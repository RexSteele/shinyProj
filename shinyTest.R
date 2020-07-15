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
        column(3, title = "workingSet1", paste(getPlayer()[1:4], collapse = "")),
        column(3, title = "workingSet2", paste(getPlayer()[5:8], collapse = "")),
        column(3, title = "workingSet3", paste(getPlayer()[9:12], collapse = "")),
        column(3, title = "workingSet4", paste(getPlayer()[13:16], collapse = ""))
      ),
      
      #Cryptex Image
      img(src="cryptex2.png", alight = "center", height = "300px", width = "500px")

    )
  )
)

# Define server
server <- function(input, output) {
  
  # Observe click of shuffle all
  playerVector <- eventReactive(input$shuffleAll, {
    shuffleAll()
    output$workingSet1 <- verbatimTextOutput({paste(getPlayer[1:4], collapse = "")})
  })
}

shinyApp(ui = ui, server = server)
