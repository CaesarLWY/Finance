
options(shiny.sanitize.errors = FALSE)
library(shiny)

shinyUI(fluidPage(
  navbarPage(inverse= TRUE, "Investments",
             tabPanel("Stock",
                      sidebarPanel(
                        radioButtons("tech","techincal analysis",
                                     c("Do Not Show","MA","MACD","Bollinger Bands","RSI")),
                        textInput("stockid","Stock ID:",value="AAPL")),
                        sidebarPanel(selectInput("interval","Interval:",c("5 days","10 days","20 days","60 days","120 days","240 days"))),
                      mainPanel(h2("Stock Price"),plotOutput("plot"),tableOutput("tab"))),
             tabPanel("S&P 500",
                      sidebarPanel(
                       dateRangeInput("dates","Date Range:",start= "2015-01-01", end=as.character(Sys.Date()))),
                      mainPanel(h2("S&P 500"),plotOutput("plot2"),tableOutput("tab2"))),
             tabPanel("TSEC Weighted Index",
                      sidebarPanel(
                        dateRangeInput("dates2","Date Range:",start= "2015-01-01", end=as.character(Sys.Date()))),
                      mainPanel(h2("TSEC Weighted Index"),plotOutput("plot3"),tableOutput("tab3"))),
             tabPanel("Machine Learning Prediction",
                      mainPanel(h1("Coming Soon.")))
             )))
       
                    
                      
  
