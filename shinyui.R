library(shiny)
source("RandGen.R")

ui <- shinyUI(bootstrapPage(
  absolutePanel(
    selectInput("dropdown", label = 'SelectInput', choices = c('A', 'B')),
    textOutput("renderText1")
  )
)
)

server <- function(input,output,session)
{
  cab <- eventReactive(input$dropdown, {
    query <-  sprintf("select ....",input$dropdown)
    #cabinet_info <- dbGetQuery(con,query)  #Replaced by a constant
    cabinet_info <- paste(c(input$dropdown, 'a','w','r','t'), sep=",")
  })

  output$renderText1 <- renderText({
    cab()
  })
}

shinyApp(ui, server)
