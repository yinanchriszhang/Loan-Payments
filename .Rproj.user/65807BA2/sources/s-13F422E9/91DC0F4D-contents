library(shiny)

ui <- fluidPage(
  actionButton("go", "Go"),
  numericInput("n", "n", 50),
  plotOutput("plot"),
  dataTableOutput("table")
)

server <- function(input, output) {
  
  randomVals <- eventReactive(input$go, {
    rnorm(input$n)
  })
  
  output$plot <- renderPlot({
    x <- randomVals()
    hist(x)
  })
  output$table <- renderDataTable(randomVals(),
                                  options = list(
                                    pageLength = 1,
                                    initComplete = I("function(settings, json) {alert('Done.');}"))
  )
}

shinyApp(ui, server)

