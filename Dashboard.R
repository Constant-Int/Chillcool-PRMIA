library(shiny)
library(shinydashboard)
library("zoo")
library("rugarch")
library(xts)
library(fOptions)
library(lubridate)
library(ggplot2)
library(DT)
library(timeSeries)
library(PerformanceAnalytics)
library(fPortfolio)

ui <- dashboardPage(
  dashboardHeader(title = "Risk Analysis Dashboard"),
  dashboardSidebar(
    selectInput("Asset", "Asset Type",
                c("USDAUD(European)" = "USDAUD(European)")),
    fileInput('file', 'Choose CSV File as Data Input:',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    numericInput("ST", "Strike Price:",min = -5, max = 9, value = 1.6, step = 0.01),
    dateRangeInput("daterange", "Date range:",
                   start  = "1999-01-04",
                   end    = "2004-12-31",
                   min    = "1999-01-04",
                   max    = "2004-12-31",
                   format = "mm/dd/yy",
                   separator = " - "),
    dateInput("treedate", "Enter Date (View the Binomial Tree):", value = "1999-01-04",min = "1999-01-04",max = "2004-12-31"),
    numericInput("rf", "Risk Free Rate for Hedging Strategy:",min = -100, max = 100, value = 0, step = 0.1),
    selectInput("Model", "Model:",
                c("Garch(1,1)" = "Garch(1,1)")),
    selectInput("Expiry", "Expiry:",
                c("1M" = "1M"))
  ),
  dashboardBody(
    fluidPage(
      tabBox(
        title = "Results",
        id = "tabset1", height = "600px",width="360px",
        tabPanel("Forecast",
                 box(title = "VaR forecast(mean value):", status = "primary", solidHeader = TRUE,
                     height = "100px",width="100px",textOutput("forecast"))
        ),
        tabPanel("VaR",
                 box(title = "VaR", status = "primary", solidHeader = TRUE,
                     height = "500px",width="380px",plotOutput("plotVaR", height = "480px",width="550px"))
        ),
        tabPanel("Volatility",
                 box(title = "Volatility", status = "primary", solidHeader = TRUE,
                     height = "500px",width="380px",plotOutput("plotVol", height = "480px",width="550px"))
        ),
        tabPanel("Delta",
                 box(title = "Delta", status = "primary", solidHeader = TRUE,
                     height = "500px",width="380px",plotOutput("plotdelta", height = "480px",width="550px"))
        ),
        tabPanel("Gamma",
                 box(title = "Gamma", status = "primary", solidHeader = TRUE,
                     height = "500px",width="380px",plotOutput("plotgamma", height = "480px",width="550px"))
        ),
        tabPanel("Theta",
                 box(title = "Theta", status = "primary", solidHeader = TRUE,
                     height = "500px",width="380px",plotOutput("plottheta", height = "480px",width="550px"))
        ),
        tabPanel("Vega",
                 box(title = "Vega", status = "primary", solidHeader = TRUE,
                     height = "500px",width="380px",plotOutput("plotvega", height = "480px",width="550px"))
                 
        ),
        tabPanel("Tree",
                 box(title = "Binomila tree model", status = "primary", solidHeader = TRUE,
                     height = "500px",width="380px",plotOutput("plottree", height = "480px",width="550px"))
        ),
        tabPanel("Hedging Strategy (Efficient Frontier)",
                 box(title = "Efficient Frontier (Gold)", status = "primary", solidHeader = TRUE,
                     height = "500px",width="380px",plotOutput("plotEF", height = "480px",width="550px"))
                 
        ),
        tabPanel("Hedging Strategy (Proportion)",
                 box(title = "Proportion (Gold):", status = "primary", solidHeader = TRUE,
                     height = "100px",width="100px",textOutput("HS"))
        )
      )
    )
  )
)

server <- function(input, output) {
  output$plotdelta <- renderPlot({
    intc=read.csv(input$file$datapath, header = TRUE, sep= ",")
    SDate<-as.Date(as.character(input$daterange[1]))
    EDate<-as.Date(as.character(input$daterange[2]))
    int<-interval(SDate,EDate)
    intc<-intc[ymd(intc$'ï..Date') %within% int,]
    intc_delta<-zoo(GBSGreeks(Selection = 'delta',TypeFlag = "p",S = intc[,6],X = input$ST,r =intc[,3],Time =7/365,sigma = intc[,5],b = 0.00))
    index(intc_delta)<-as.Date(intc[,"ï..Date"])
    plot(intc_delta,type="l",main="Delta",xlab="Date",ylab="Delta",col='red')
  })
  output$plotgamma <- renderPlot({
    intc=read.csv(input$file$datapath, header = TRUE, sep= ",")
    SDate<-as.Date(as.character(input$daterange[1]))
    EDate<-as.Date(as.character(input$daterange[2]))
    int<-interval(SDate,EDate)
    intc<-intc[ymd(intc$'ï..Date') %within% int,]
    intc_gamma<-zoo(GBSGreeks(Selection = 'gamma',TypeFlag = "p",S = intc[,6],X = input$ST,r =intc[,3],Time =7/365,sigma = intc[,5],b = 0.00))
    index(intc_gamma)<-as.Date(intc[,"ï..Date"])
    plot(intc_gamma,type="l",main="Gamma",xlab="Date",ylab="Gamma",col='orange')
  })
  output$plottheta <- renderPlot({
    intc=read.csv(input$file$datapath, header = TRUE, sep= ",")
    date <- as.Date(intc$'ï..Date')
    SDate<-as.Date(as.character(input$daterange[1]))
    EDate<-as.Date(as.character(input$daterange[2]))
    int<-interval(SDate,EDate)
    intc<-intc[ymd(intc$'ï..Date') %within% int,]
    intc_theta<-zoo(GBSGreeks(Selection = 'theta',TypeFlag = "p",S = intc[,6],X = input$ST,r =intc[,3],Time =7/365,sigma = intc[,5],b = 0.00))
    index(intc_theta)<-as.Date(intc[,"ï..Date"])
    plot(intc_theta,type="l",main="Theta",xlab="Date",ylab="Theta",col='darkgreen')
  })
  output$plotvega <- renderPlot({
    intc=read.csv(input$file$datapath, header = TRUE, sep= ",")
    SDate<-as.Date(as.character(input$daterange[1]))
    EDate<-as.Date(as.character(input$daterange[2]))
    int<-interval(SDate,EDate)
    intc<-intc[ymd(intc$'ï..Date') %within% int,]
    intc_vega<-zoo(GBSGreeks(Selection = 'vega',TypeFlag = "p",S = intc[,6],X = input$ST,r =intc[,3],Time =7/365,sigma = intc[,5],b = 0.00))
    index(intc_vega)<-as.Date(intc[,"ï..Date"])
    plot(intc_vega,type="l",main="Vega",xlab="Date",ylab="Vega",col='darkblue')
  })
  output$plottree<- renderPlot({
    intc=read.csv(input$file$datapath, header = TRUE, sep= ",")
    SDate<-as.Date(as.character(input$treedate))
    EDate<-as.Date(as.character(input$treedate))
    int<-interval(SDate,EDate)
    intc<-intc[ymd(intc$'ï..Date') %within% int,]
    CRRTREE<-BinomialTreeOption(TypeFlag = "pe",S = intc[1,6],X = input$ST,r =intc[1,3],Time =7/365,sigma = intc[1,5],b = 0.00,n = 7)
    BinomialTreePlot(CRRTREE,dy = 1,xlab = "Times steps",ylab = "Number of up steps",xlim = c(0,8),ylim=c(-8,8))
    title(main = "Put Option Tree")
  })
  output$plotVol<- renderPlot({
    intc=read.csv(input$file$datapath, header = TRUE, sep= ",")
    SDate<-as.Date(as.character(input$daterange[1]))
    EDate<-as.Date(as.character(input$daterange[2]))
    int<-interval(SDate,EDate)
    intc<-intc[ymd(intc$'ï..Date') %within% int,]
    intc.x <- xts(x=intc[,"Return"],order.by=as.Date(intc[,"ï..Date"]))
    intc_garch11_spec<-ugarchspec(variance.model = list(
      garchOrder=c(1,1)),
      mean.model = list(armaOrder=c(0,0)))
    intc_garch11_roll<-ugarchroll(intc_garch11_spec,intc.x,
                                  n.start=120,refit.every=1,refit.window="moving",
                                  solver="hybrid",calculate.VaR = TRUE, Var.alpha=0.05,
                                  keep.coef=TRUE)
    intc_Vol<-zoo(intc_garch11_roll@forecast$density[,2])
    intc_Vola<-zoo(intc_garch11_roll@forecast$density[,6])
    index(intc_Vola)<-as.Date(rownames(intc_garch11_roll@forecast$VaR))
    index(intc_Vol)<-as.Date(rownames(intc_garch11_roll@forecast$VaR))
    plot(intc_Vola,type="b",main="95% 1 Day Volatility Backtesting",
         xlab="Date",ylab="Volatility")
    lines(intc_Vol,col="red")
    lines(-intc_Vol,col="red")
  })
  output$plotVaR<- renderPlot({
    intc=read.csv(input$file$datapath, header = TRUE, sep= ",")
    SDate<-as.Date(as.character(input$daterange[1]))
    EDate<-as.Date(as.character(input$daterange[2]))
    int<-interval(SDate,EDate)
    intc<-intc[ymd(intc$'ï..Date') %within% int,]
    intc.x <- xts(x=intc[,"Return"],order.by=as.Date(intc[,"ï..Date"]))
    intc_garch11_spec<-ugarchspec(variance.model = list(
      garchOrder=c(1,1)),
      mean.model = list(armaOrder=c(0,0)))
    intc_garch11_roll<-ugarchroll(intc_garch11_spec,intc.x,
                                  n.start=120,refit.every=1,refit.window="moving",
                                  solver="hybrid",calculate.VaR = TRUE, Var.alpha=0.05,
                                  keep.coef=TRUE)
    intc_VaR <- zoo(intc_garch11_roll@forecast$VaR[,2])
    index(intc_VaR)<-as.Date(rownames(intc_garch11_roll@forecast$VaR))
    intc_actual<-zoo(intc_garch11_roll@forecast$VaR[,3])
    index(intc_actual)<-as.Date(rownames(intc_garch11_roll@forecast$VaR))
    plot(intc_actual,type="b",main="95% 1 Day VaR Backtesting",
         xlab="Date",ylab="Return/VaR in percent")
    lines(intc_VaR,col="red")
  })
  output$forecast<-renderText({
    inFile <- input$file
    if (!is.null(inFile))
      intc=read.csv(inFile$datapath, header = TRUE, sep= ",")
    SDate<-as.Date(as.character(input$daterange[1]))
    EDate<-as.Date(as.character(input$daterange[2]))
    int<-interval(SDate,EDate)
    intc<-intc[ymd(intc$'ï..Date') %within% int,]
    intc.x <- xts(x=intc[,"Return"],order.by=as.Date(intc[,"ï..Date"]))
    intc_garch11_spec<-ugarchspec(variance.model = list(
      garchOrder=c(1,1)),
      mean.model = list(armaOrder=c(0,0)))
    intc_garch11_fit<-ugarchfit(spec = intc_garch11_spec,
                                data=intc.x)
    intc_garch11_fcst<-ugarchforecast(intc_garch11_fit,n.ahead=12)
    s<-sigma(intc_garch11_fcst)
    qnorm(0.95)*mean(s)
  })
  output$plotEF<- renderPlot({
    intc=read.csv(input$file$datapath, header = TRUE, sep= ",")
    IT<-timeSeries(intc[,6:7],intc[,"ï..Date"])
    IT_return<-returns(IT)
    Spec=portfolioFrontier(IT_return)
    Spec=portfolioSpec()
    setSolver(Spec)="solveRshortExact"
    Frontier<-portfolioFrontier(as.timeSeries(IT_return),
                                Spec,constraints="Short")
    frontierPlot(Frontier,col=rep('orange',2),pch=19)
    monteCarloPoints(Frontier,mcSteps=1000,cex=0.25,pch=19,col="red")
  })
  output$HS<-renderText({
    intc=read.csv(input$file$datapath, header = TRUE, sep= ",")
    return <- data.frame("AUDUSD"=intc[,"Return"],"Gold"=intc[,"Gold"])
    assets<- data.frame("assetprice"=intc[,"Price"],"goldprice"=intc[,"Goldprice"])
    Q<-cbind(cov(return),rep(0,ncol(assets)))
    n<-ncol(assets)+1
    Q<-rbind(Q,rep(0,n))
    mu<-0.005
    rf<-input$rf/100
    r<-c(colMeans(return),rf)
    Q1<-rbind(Q,rep(1,n),r)
    Q1<-cbind(Q1,rbind(t(tail(Q1,2)),matrix(0,2,2)))
    b<-c(rep(0,n),1,mu)
    c<-solve(Q1,b)
    combination<-head(c,-3)
    result<-combination/sum(combination)
    paste("USDAUD = ",result[1]," , Gold = ",result[2])
  })
}

shinyApp(ui, server)
