2
shinyServer(function(input,output){
  library(quantmod)
  #library(Qaundl)
  library(ggplot2)
  output$plot <- renderPlot({
    stock_price = getSymbols(input$stockid, auto.assign = FALSE)
    data = switch(input$interval,
                  "5 days"= stock_price[((nrow(stock_price)-5):nrow(stock_price)),],
                  "10 days"= stock_price[((nrow(stock_price)-10):nrow(stock_price)),],
                  "20 days"= stock_price[((nrow(stock_price)-20):nrow(stock_price)),],
                  "60 days"= stock_price[((nrow(stock_price)-60):nrow(stock_price)),],
                  "120 days"= stock_price[((nrow(stock_price)-120):nrow(stock_price)),],
                  "240 days"= stock_price[((nrow(stock_price)-240):nrow(stock_price)),])
   if(input$tech=="Do Not Show"){chartSeries(data, theme= "white")}
    else if(input$tech=="MA"){chartSeries(data,theme="white") 
        plot(addSMA(n=5))}
    else if(input$tech=="MACD"){
      if(input$interval %in% c("5 days","10 days","20 days")){
        plot(x=10,y=10,main="Can not show less than 26 days.", xaxt= "n", yaxt= "n", xlab="",ylab="", type="n")}
      else{chartSeries(data, theme= "white")
        plot(addMACD())}}
    else if(input$tech=="RSI"){
      if(input$interval %in% c("5 days","10 days")){
        plot(x=10, y=10, main= "Can not show less than 14 days.",xaxt="n", yaxt= "n", xlab="",ylab="",type="n")}
      else{chartSeries(data, theme="white")
        addRSI()}}
    else if(input$tech=="Bollinger Bands"){
      if(input$interval %in% c("5 days", "10 days")){
        plot(x=10, y=10, main= "Can not show less than 20 days.",xaxt="n", yaxt= "n", xlab="",ylab="",type="n")}
        
      else{chartSeries(data, theme= "white")
        addBBands()}}
  })
  output$tab <- renderTable({
    stock_price = getSymbols(input$stockid, auto.assign = FALSE)
    data = switch(input$interval,
                  "5 days"= stock_price[((nrow(stock_price)-5):nrow(stock_price)),],
                  "10 days"= stock_price[((nrow(stock_price)-10):nrow(stock_price)),],
                  "20 days"= stock_price[((nrow(stock_price)-20):nrow(stock_price)),],
                  "60 days"= stock_price[((nrow(stock_price)-60):nrow(stock_price)),],
                  "120 days"= stock_price[((nrow(stock_price)-120):nrow(stock_price)),],
                  "240 days"= stock_price[((nrow(stock_price)-240):nrow(stock_price)),])
    print(data)
  })
  output$plot2 <- renderPlot({
    SP500 = getSymbols("^GSPC", auto.assign = FALSE, from= input$dates[1], to= input$dates[2])
    chartSeries(SP500, theme= "white")
    })
  output$tab2 <- renderTable({
    SP500 = getSymbols("^GSPC", auto.assign = FALSE, from= input$dates[1], to= input$dates[2])
    print(SP500)
  })
  output$plot3 <- renderPlot({
    TWII = getSymbols("^TWII", auto.assign= FALSE, from= input$dates2[1], to= input$dates2[2])
    chartSeries(TWII, theme= "white")
  })
  output$tab3 <- renderTable({
    TWII = getSymbols("^TWII", auto.assign= FALSE, from= input$dates2[1], to= input$dates2[2])
    print(TWII)
  })
})
