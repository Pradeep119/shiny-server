
library(rsconnect)
library(shiny)

shinyServer(function(input, output) {
  
  inputData<-reactive({
    
    inFile <- input$data
    
    if (is.null(inFile))
      return(NULL)
    
    file <- read.csv(inFile$datapath, header = input$header,
                     sep = input$sep, quote = input$quote)
    
  })
  
  output$table <- renderTable({
    inFile <- input$data
    
    if(is.null(input$data))  {
      h5("No any data")
      return(NULL)
    } 
    file <- read.csv(inFile$datapath, header = input$header,
                     sep = input$sep, quote = input$quote)
    
    file<- file[1:input$obs,]
    
    
  })
  
  
  
  output$plot <- renderPlot({
    
    inFile <- input$data
    
   
      if(is.null(input$data))    
        return(NULL) 
      file <- read.csv(inFile$datapath, header = input$header,
                       sep = input$sep, quote = input$quote)
      
      file<- file[1:input$obs,]
      x<- file[,1]
      y<- file[,2]
     
    
    
    
    a<- input$intercept
    b<-input$slope
    #    
    #    yhat <- input$intercept + x * input$slope
    regression <- lm(y~x)
    intercept1<- coef(regression)["(Intercept)"] 
    slope<- coef(regression)["y"] 
    #   
    
    plot(x, y, cex = 1, font = 3)
    points(x, y, pch = 16, cex = 0.8, col = "red",,xlab = "Explanatory Variable",ylab = "Outcome Variable")
    title("Linear Regression")
    #abline(a = coef(ckd_model)[1], b = coef(intercept1)[2], lty = 2, lwd = 2, col = "red")
    abline(intercept = intercept1, slope = slope, colour = "red", size = 2) 
    
  })
  
  
  
  output$LinearPlot <- renderPlot({
    
    inFile <- input$data
    
      file <- read.csv(inFile$datapath, header = input$header,
                       sep = input$sep, quote = input$quote)
      
      file<- file[1:input$obs,]
      x<- file[,1]
      y<- file[,2]
    
    
    
   # par(mfrow=c(2,2))
    plot(y ~ x, col = rep(c("red", "blue"), each = 50), pch = 16, xlab = "x (Predictor variable)", 
         ylab = "y (output variable) ")
    
    
    ckd_model <- lm(y ~ x )
    abline(a = coef(ckd_model)[1], b = coef(ckd_model)[2], lty = 2, lwd = 2, col = "red")
   
  })
  
  
  
  output$summary <-renderPrint({
    inFile <- input$data
    
    
    file <- read.csv(inFile$datapath, header = input$header,
                     sep = input$sep, quote = input$quote)
    
    file<- file[1:input$obs,]
    
    
    summary(file)
    
  })
  
  
  output$Linearsummary <-renderPrint({
    inFile <- input$data
    
    
   file <- read.csv(inFile$datapath, header = input$header,
                     sep = input$sep, quote = input$quote)
    
    file<- file[1:input$obs,]
    x<- file[,1]
    y<- file[,2]
    
    
    
    summary(lm(y~x))
    
  })
  
  
  
  
  
  
  
})

