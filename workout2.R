#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(reshape2)

choices = c("Yes", "No")

mylist <- as.list(choices)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Finances!"),
  
  fluidRow(
    
    column(4,
           sliderInput("init",
                       "Initial Amount",
                       min = 0,
                       max = 100000,
                       value = 1000) 
    ),
    column(4,
           sliderInput("rr",
                       "Return Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 5) 
           
    ),
    column(4,
           sliderInput("yr",
                       "Years",
                       min = 0,
                       max = 50,
                       value = 10)
           
    )),
  fluidRow(
    
    column(4,
           sliderInput("annual",
                       "Annual Contribution",
                       min = 0,
                       max = 50000,
                       value = 1000) 
    ),
    column(4,
           sliderInput("gr",
                       "Growth Rate (in %)",
                       min = 0,
                       max = 20,
                       value = 1) 
           
    ),
    column(4,
           selectInput(
             "facetBoo",
             label = h3("Facet?"),
             choices = mylist
           )
           
    )),
  tags$hr(),
  h4('Timelines'),
  plotOutput('distPlot'),
  h4('Balances'),
  tableOutput('balance_table')

  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    initialAmount <- input$init
    returnRate <- (input$rr)/100
    yearsIn <- input$yr
    annualContrib <- input$annual
    growthRate <- (input$gr)/100
    facet <- input$facetBoo 
    future_value <- function(amount, rate, years) {
      secondPart <- (1 + rate)^years 
      returnVal <- amount * secondPart
      return(returnVal)
    }
    annuity <- function(contrib, rate, years) {
      secondVal <- (1 + rate)^years - 1
      secondTrueVal <- secondVal/rate
      returnVal <- contrib * secondTrueVal
      return(returnVal)
    }
    growing_annuity <- function(contrib, rate, growth, years) {
      numerator <- (1 + rate)^years - (1+growth)^years
      denom <- rate - growth 
      returnVal <- contrib * numerator/denom
      return(returnVal)
    }
    FV <- c()
    FVA <- c()
    FVAG <- c()
    for (i in 1:(yearsIn + 1))
      FV[i] <- future_value(amount = initialAmount, rate = returnRate, years = i-1)
    for (i in 1:(yearsIn + 1))
      FVA[i] <- annuity(contrib = annualContrib, rate = returnRate, years = i-1) + future_value(amount = initialAmount, rate = returnRate, years = i-1)
    for (i in 1: (yearsIn + 1))
      FVAG[i] <- growing_annuity(contrib = annualContrib, rate = returnRate, growth = growthRate, years = i-1) + future_value(amount = initialAmount, rate = returnRate, years = i-1)
    years <- c(0:yearsIn)
    bubba <- data.frame(years = years, noContrib = FV, fixedContrib = FVA, growingContrib = FVAG)
    
    if (input$facetBoo == "Yes") {    
      dataMelt <- reactive({
        melt(bubba, id.vars = 'years', value.name = 'values')
      })
      bubbaFac <- ggplot(data = dataMelt(), aes(x = years, y = values)) + geom_area(aes(fill = variable)) +
        geom_point(aes(color = variable)) + geom_line(aes(color = variable)) + facet_wrap(~variable)
      bubbaFac
    }
    else if (input$facetBoo == "No"){
      ggplot() + 
      geom_line(data = bubba, aes(x = years, y = FVA), color = "blue") +
      geom_line(data = bubba, aes(x = years, y = FV), color = "red") +
      geom_line(data = bubba, aes(x = years, y = FVAG), color = "green") +
      xlab('Years') +
      ylab('value')
    }
    

  
    }
    

    
    
    
  #}
  )
  output$balance_table <- renderTable({
    initialAmount <- input$init
    returnRate <- input$rr
    yearsIn <- input$yr
    annualContrib <- input$annual
    growthRate <- input$gr
    facet <- input$facetBoo 
    future_value <- function(amount, rate, years) {
      secondPart <- (1 + rate)^years 
      returnVal <- amount * secondPart
      return(returnVal)
    }
    annuity <- function(contrib, rate, years) {
      secondVal <- (1 + rate)^years - 1
      secondTrueVal <- secondVal/rate
      returnVal <- contrib * secondTrueVal
      return(returnVal)
    }
    growing_annuity <- function(contrib, rate, growth, years) {
      numerator <- (1 + rate)^years - (1+growth)^years
      denom <- rate - growth 
      returnVal <- contrib * numerator/denom
      return(returnVal)
    }
    FV <- c()
    FVA <- c()
    FVAG <- c()
    for (i in 1:(yearsIn + 1))
      FV[i] <- future_value(amount = initialAmount, rate = returnRate, years = i-1)
    for (i in 1:(yearsIn + 1))
      FVA[i] <- annuity(contrib = annualContrib, rate = returnRate, years = i-1) + future_value(amount = initialAmount, rate = returnRate, years = i-1)
    for (i in 1: (yearsIn + 1))
      FVAG[i] <- growing_annuity(contrib = annualContrib, rate = returnRate, growth = growthRate, years = i-1) + future_value(amount = initialAmount, rate = returnRate, years = i-1)
    years <- c(0:yearsIn)
    modalities <- data.frame(years = years, noContrib = FV, fixedContrib = FVA, growingContrib = FVAG)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

