library(shiny)
library(shinyjs)
library(forecast)
library(tseries)
library(lubridate)
library(zoo)
library(lattice)
library(DT)
library(TSclust)
library(imputeTS)
library(vars)
library(tseries)
library(timeSeries)
library(ggplot2)
library(reshape2)

##importing data
Fed<- read.csv("FederalReserveInterestRates.csv", skip=5,as.is=TRUE)
Gold<- read.csv("GOLDmonthly_csv.csv",as.is=TRUE)
SP<- read.csv("S&Pdata_csv.csv",colClasses= c(NA,NA, rep("NULL",8)),as.is=TRUE)

##renaming column name in the Fed database
names(Fed) <- c("Date", "Interest")

##Converting Date from format to Date

Fed$Date<-paste(1, Fed$Date)
Gold$Date<-paste(1, Gold$Date)

SP$Date<-as.Date(SP$Date,"%Y-%m-%d")
Gold$Date<-as.Date(Gold$Date, "%d %Y-%m")

################################################################

Fed$Date<-as.Date(Fed$Date,"%d %b-%y")

## MERGE
all<- merge(Gold, SP, by="Date")
all<- merge(all, Fed, by="Date")

class(all)
sapply(all, class)





shinyServer(function(input, output, session){

  hideTab("tabPanels", target = "compPanel" )
  hideTab("tabPanels", target = "forecastPanel" )
  hideTab("tabPanels", target = "summaryPanel" )
  
  observeEvent(input$go, {
    
    showTab("tabPanels", target = "compPanel")
    showTab("tabPanels", target = "forecastPanel" )
    showTab("tabPanels", target = "summaryPanel" )
    
    
    
    output$dt <- DT::renderDataTable({
      DT::datatable(all[,input$selection, drop = FALSE])
    })
    
    
    output$compPlot <- renderPlot({
      
      plot(allTS[,input$selection, drop = FALSE])
    }) 
    
    
    allTS <- ts(all[,input$selection], frequency = 12, start = c(1969, 1))
    
    
    all_diff <- diff(allTS)
    varmodel <- VAR(all_diff, lag.max = 4, ic = "AIC")
    
    prediction <- predict(varmodel, n.ahead = input$slider, ci =0.95)
    
    output$forecastPlot <- renderPlot({
      fanchart(prediction)
    
      
    })  
    
    output$sum <- renderPrint({
    
      print(prediction)
    })
      })
  
  
  
  observeEvent(input$reset, {
    
    updateCheckboxGroupInput(session, "selection", selected = FALSE )
    hideTab("tabPanels", target = "compPanel" )
    hideTab("tabPanels", target = "forecastPanel" )
    hideTab("tabPanels", target = "summaryPanel" )
    updateSliderInput(session, "slider", value = 6)
    
  })
  })
