

library(shiny)
library(shinydashboard)
library(FinancialMath)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(scales)

## app.R ##

ui <- dashboardPage(
    dashboardHeader(title = "Loan Repayment"),
    dashboardSidebar(disable = T),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(width = 3,
                title = "Controls",
                #Run button
                actionButton("run", "Calculate", icon("refresh")),
                helpText("Press to run the calculator based on the criterias given.  To refresh, press again."),
                #Price of asset
                numericInput("AssetPrice", "Total Asset Value in $:", 600000, min = 1),
                helpText("What is the total price that you paid for the house/car/boat?"), 
                
                #Interest Rate input
                numericInput("Interest", "Interest Rate in %", 3.75, min = 0, max = 30, step = 0.01),
                helpText("Interest reate you are paying.  Eg, If the interest rate is 3.75% enter 3.75"),
                
                #LVR input
                sliderInput("LVR", "Loan to Value Ratio", 30, 100, 70, step = 5, post = "%"),
                helpText("What % of the total asset value can you borrow"),
                
                #Length of loan input
                numericInput("duration", "Loan duration (in years)", 30, min=0.5, max=30, step=0.5),
                helpText("The total duration of the loan in years"),
                
                #Drop down box for how often are you  repaying
                #Interest compounding is calculated on this criteria also.
                selectInput("pfreq", "How often are you repaying the loan?",
                                   c("Weekly" = 365.25/7,
                                     "Fortnightly" = 365.25/7/2,
                                     "Monthly" = 12,
                                     "Quarterly" = 4,
                                     "Half Yearly" = 2,
                                     "Yearly" = 1),
                            selected = 12)
               
            ),
            
            #Loan summary
            box(width = 3,
                h4("Loan Amount:", textOutput("LoanAmount")),
                h4("Deposit required:", textOutput("Deposit")),
                h4("Repayments for each period are:", textOutput("Repayment"))
            ),
            
            #output plots
            box(plotlyOutput("plot1"))
            
            )
        )
    )


server <- function(input, output, session) {
    #Calculate loan amount 
    LA <- eventReactive(input$run, {
        LoanAmount <- input$AssetPrice*input$LVR/100
        paste('$',formatC(LoanAmount, big.mark=',', format = 'f', digits = 0)) 
    })
    output$LoanAmount <- renderText({
        LA()
    })   
    
    #Calculate deposit required
    Dep <- eventReactive(input$run, {
        Deposit <- input$AssetPrice*(1-input$LVR/100)
        paste('$',formatC(Deposit, big.mark=',', format = 'f', digits = 0))
    })
    output$Deposit <- renderText({
        Dep()
    }) 
    
    #Calculate value of each repayment
    Repay <- eventReactive(input$run, {
        x <- amort.period(Loan = input$AssetPrice*input$LVR/100, 
                          n = input$duration*round(as.numeric(input$pfreq),0), 
                          i = input$Interest/100, 
                          ic = as.numeric(input$pfreq), 
                          pf = as.numeric(input$pfreq))[2,]
        paste('$',formatC(x, big.mark=',', format = 'f', digits = 0))
    }) 
    output$Repayment <- renderText({
        Repay()
    }) 
    
    #Calculate the data table to create the graph from.
    x1 <- eventReactive(input$run,{
        x <- amort.table(Loan = input$AssetPrice*input$LVR/100, 
                         n = input$duration*round(as.numeric(input$pfreq),0), 
                         i = input$Interest/100, 
                         ic = as.numeric(input$pfreq), 
                         pf = as.numeric(input$pfreq))
        x1 <- x$Schedule %>% as.data.frame()
    })
  
#Creating plot  
    output$plot1 <- renderPlotly({
        
        p1 <- ggplot(data = x1(), mapping = aes(x = Year)) + 
            geom_line(aes(y = Balance), size=1, colour="darkblue") + 
            theme_light() + 
            labs(title = "Balance of Loan, Principle and Interest Amount",
                 y = NULL) +
            scale_y_continuous(labels = dollar_format())
        x2 <- x1() %>% gather(key = PI, value = Amount, `Interest Paid`, `Principal Paid`) %>% 
            select(Year, PI, Amount)
        p2 <- ggplot(data = x2, aes(x = Year, y = Amount))+ 
            geom_col(aes(fill = PI)) + 
            theme_light() + 
            scale_y_continuous(labels = dollar_format())
        
   #Using subplot with plotly to message two graphs into one window     
        subplot(ggplotly(p1), ggplotly(p2), shareX = TRUE, nrows = 2)
        
    })

}

shinyApp(ui, server)