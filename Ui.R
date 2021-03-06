library(shiny)
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

all<- merge(Gold, SP, by="Date")
all<- merge(all, Fed, by="Date")

shinyUI(fluidPage(
  
  titlePanel("Time Series Analysis"),
  
  sidebarLayout(
    
    
    sidebarPanel(
    
      checkboxGroupInput("selection", "Select the commodoties you want to explore", names(all[]), selected = "Date"),
      sliderInput("slider", min = 1, max = 12, value = 6, label = "Number of Months"),
      actionButton("go", "Analyze", width = "100%", class="btn btn-primary"),
      p(),              
      actionButton("reset", "Reset", width = "100%", class="btn btn-warning"),
      p()       
      
    ),
    
    
    
mainPanel(
  
    tabsetPanel(id="tabPanels",
                tabPanel("Data", value = "datapanel", DT::dataTableOutput('dt')),
                tabPanel("Comparision", value="compPanel", plotOutput("compPlot")),
                tabPanel("Forecast", value="forecastPanel", plotOutput("forecastPlot")),
                tabPanel("Summary", value = "summaryPanel", verbatimTextOutput("sum"))
           
                     
  
))

)))
