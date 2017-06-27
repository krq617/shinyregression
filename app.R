library(shiny)
library(ggplot2)
myData <- NULL
# background color "#D3D3D3"
ui <- fluidPage(  fluidRow(column(4, column(11, offset = 1, fluidRow(tags$h2("Linear Regression Tools")),
                                            fluidRow(style = "border-style: solid;background-color:#D3D3D3;border-color: #000000;",column(6,
                                                                                                                                          numericInput(inputId = "numpts",label = "Enter a Number of Points",value = 30, min = 2,max = 1000, step = 10),
                                                                                                                                          numericInput(inputId = "slope",label = "Enter a Slope",value = 1, min = -1000,max = 1000, step = 1),
                                                                                                                                          radioButtons(inputId = "rb1", label = "Residual Equal Variance?", choices = c("Yes" = "yes","No, Fan Right" = "nofr","No, Fan Left"="nofl"))
                                            ),
                                            column(6,
                                                   numericInput(inputId = "error",label = "Enter a Standard Deviation",value = 1, min = 0,max = 10, step = 0.1),
                                                   radioButtons(inputId = "rb2", label = "Linear Relationship?", choices = c("Yes" = "yes","No, Quadratic" = "noqu","No, oscillating "="nofl")),
                                                   radioButtons(inputId = "rb3", label = "Independent?", choices = c("Yes" = "yes","No" = "nod"))
                                            )
                                            )
)),
column(4,plotOutput(outputId = "basicreg",
                    click = "regclick",
                    brush = brushOpts(
                      id = "regbrush"
                    ))
),
column(4,plotOutput(outputId = "topright"))
), 
fluidRow(
  
  column(4, column(11, offset = 1,style = "border-style: solid;background-color:#D3D3D3;border-color: #000000;", 
                   fluidRow(column(6,selectInput(inputId = "tlp",label = "Top Left Plot", choices = c("Scatterplot + Regression"="notusedanywhereatall"))),
                            column(6,selectInput(inputId = "trp",label = "Choose the Top Right Plot", choices = c("QQ-Plot"="qq","Residual vs. Fit"="rvf","Residuals vs. Order"="rvo","Histogram of Residuals"= "eh","Cook's Distance vs. Leverage" = "cvl")))
                   ),
                   fluidRow(column(6,selectInput(inputId = "blp",label = "Choose the Bottom Left Plot", choices = c("QQ-Plot"="qq","Residual vs. Fit"="rvf","Residuals vs. Order"="rvo","Histogram of Residuals" = "eh","Cook's Distance vs. Leverage" = "cvl"),selected = "rvf")),
                            column(6,selectInput(inputId = "brp",label = "Choose the Bottom Right Plot", choices = c("QQ-Plot"="qq","Residual vs. Fit"="rvf","Residuals vs. Order"="rvo","Histogram of Residuals" = "eh","Cook's Distance vs. Leverage" = "cvl"),selected = "rvo"))
                   ),
                   fluidRow(column(6,checkboxInput(inputId = "Conf", label = tags$b("Confidence Bands?"), value = FALSE)),
                   column(6,checkboxInput(inputId = "Pred", label = tags$b("Prediction Bands?"), value = FALSE))),
                   checkboxInput(inputId = "outlier", label = tags$b("Add an Outlier?"), value = FALSE),
                   fluidRow(column(6,numericInput(inputId = "outlierX",label = "Outlier X value",value = 0, min = -10,max = 10, step = 0.5)),
                            column(6,numericInput(inputId = "outlierY",label = "Outlier Y value",value = 0, min = -10,max = 10, step = 0.5))
                            ),
                   checkboxInput(inputId = "rsummary", label = tags$b("Show R Summary?"), value = FALSE)
  )),
  
  column(4,plotOutput(outputId = "bottomleft")),
  column(4,plotOutput(outputId = "bottomright"))
),
tags$br(),
fluidRow(column(4,column(11, offset = 1,style = "border-style: solid;background-color:#D3D3D3;border-color: #000000;",
                fileInput(
                  'file', 
                  'Choose file to upload.'
                ),
                
                checkboxInput(inputId = "usefile", label = tags$b("Use Uploaded File?"), value = FALSE),
                
                checkboxInput(inputId = "viewfile", label = tags$b("View Uploaded File?"), value = FALSE),
                
                selectInput(
                  "y_input", 
                  label = h5("Select Response Variable"),
                  ""
                ),
                selectInput(
                  "x_input", 
                  label = h5("Select Explanatory Variable"),
                  ""
                ))),
  column(4,verbatimTextOutput(outputId = "sumstat")),
  column(4,dataTableOutput(outputId ="dtab"))
  ),
tags$hr(style = " border: 0; height: 1px; background: #333; background-image: linear-gradient(to right, #ccc, #333, #ccc);"),
tags$a(style = "text-align: right",href = "https://krq617.github.io/kevinquinlan/","Made by Kevin Quinlan 2017")
#plotOutput(outputId = "test")
#verbatimTextOutput(outputId = "test"),
)
















##########
# Server #
##########

server <- function(input,output,session){
  addposition <- function(vector,pos){
    vector[pos] <- 2
    return(vector)
  }
  replace1 <- function(vector,value){
     vector[1] <-value
     return(vector)
  }
  
  inFile <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    } else {
      input$file
    }
  })
  
  myData <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    } else {
      read.csv(inFile()$datapath)
    }
  })
  
  observe({
    updateSelectInput(
      session,
      "y_input",
      choices=names(myData()))
  })
  observe({
    updateSelectInput(
      session,
      "x_input",
      choices=names(myData()))
  })
  
  
  ximp <- reactive({if(is.null(inFile())){
    NULL
  }
  else{
    as.vector(unlist(read.csv(inFile()$datapath)[input$x_input]))
  }
  })
  
  yimp <- reactive({if(is.null(inFile())){
    NULL
  }
  else{
    as.vector(unlist(read.csv(inFile()$datapath)[input$y_input]))
  }
  })
  
  x <- reactive({ if(input$usefile == FALSE){ if(input$outlier == TRUE){c(input$outlierX,
    runif(input$numpts,input$slope*input$error*0,10))}
    else{
      runif(input$numpts,input$slope*input$error*0,10) 
    }
  }
  else{
    if(input$outlier == TRUE){c(input$outlierX,ximp())}
    else{ximp()}
  }
    })
  e <- reactive({ rnorm(input$numpts,mean = 0*input$slope,sd = input$error)})
  y1 <- reactive({
  if(input$usefile == FALSE){  
    if(input$rb3 == "yes"){
      if(input$rb2 =="yes"){
        if(input$rb1 == "yes"){(input$slope)*x() + e()}
        else if(input$rb1 == "nofr"){(input$slope)*x() + (1.75/10)*x()*e()}
        else{(input$slope)*x() + (1.75/10)*((-x())+10)*e()}
      }
      else if(input$rb2 == "noqu"){
        if(input$rb1 == "yes"){(input$slope)*x() + 0.5*b*((x()-5)^2) + e()}
        else if(input$rb1 == "nofr"){(input$slope)*x() + 0.5*b*((x()-5)^2) + (1.75/10)*x()*e()}
        else{(input$slope)*x() + 0.5*b*((x()-5)^2) + (1.75/10)*((-x())+10)*e()}
      }
      else{
        if(input$rb1 == "yes"){(input$slope)*x() + 2*b*(sin(x())) + e()}
        else if(input$rb1 == "nofr"){(input$slope)*x() + 2*b*(sin(x())) + (1.75/10)*x()*e()}
        else{(input$slope)*x() + 2*b*(sin(x())) + (1.75/10)*((-x())+10)*e()}
        
      }
    }
    
    
    else{
      if(input$rb2 =="yes"){
        if(input$rb1 == "yes"){(input$slope)*x() + arima.sim(model=list(ar=0.9),n=input$numpts,sd = sd(e()))}
        else if(input$rb1 == "nofr"){(input$slope)*x() + (1.75/10)*x()*arima.sim(model=list(ar=0.9),n=input$numpts,sd = sd(e()))}
        else{(input$slope)*x() + (1.75/10)*((-x())+10)*arima.sim(model=list(ar=0.9),n=input$numpts,sd = sd(e()))}
      }
      else if(input$rb2 == "noqu"){
        if(input$rb1 == "yes"){(input$slope)*x() + 0.5*b*((x()-5)^2) + arima.sim(model=list(ar=0.9),n=input$numpts,sd = sd(e()))}
        else if(input$rb1 == "nofr"){(input$slope)*x() + 0.5*b*((x()-5)^2) + (1.75/10)*x()*arima.sim(model=list(ar=0.9),n=input$numpts,sd = sd(e()))}
        else{(input$slope)*x() + 0.5*b*((x()-5)^2) + (1.75/10)*((-x())+10)*arima.sim(model=list(ar=0.9),n=input$numpts,sd = sd(e()))}
      }
      else{
        if(input$rb1 == "yes"){(input$slope)*x() + 2*b*(sin(x())) + arima.sim(model=list(ar=0.9),n=input$numpts,sd = sd(e()))}
        else if(input$rb1 == "nofr"){(input$slope)*x() + 2*b*(sin(x())) + (1.75/10)*x()*arima.sim(model=list(ar=0.9),n=input$numpts,sd = sd(e()))}
        else{(input$slope)*x() + 2*b*(sin(x())) + (1.75/10)*((-x())+10)*arima.sim(model=list(ar=0.9),n=input$numpts,sd = sd(e()))}
        
      }
      
      
    }
  }
    else{
      yimp()
    }
    
  })
  
  y <- reactive({if(input$outlier == TRUE){replace1(y1(),input$outlierY)}
                else
                  y1()
                })
  reg1 <- reactive({lm(y()~x())})
  z <- reactive(data.frame(x(),y()))
  
  ###############
  #select points#
  ###############
  
  d <- reactive({brushedPoints(z(), input$regbrush,xvar = "x..", yvar = "y..")[,1]})
  dc <- reactive({rep(1,length(x()))})
  da <- reactive({1:length(x())})
  db <- reactive({(da()[(match(d(),x()))])})
  dt <- reactive({addposition(dc(),db())})
  
  ###################
  #build data frames#
  ###################
  
  coeff1 <- reactive({unlist(qqnorm(residuals(reg1()))[1])})
  coeff2 <- reactive({unlist(qqnorm(residuals(reg1()))[2])})
  coeff3 <- reactive({residuals(reg1())})
  fitv <-  reactive({fitted(reg1())})
  ord <- reactive({1:length(reg1()$residuals)})
  cookd <- reactive({cooks.distance(reg1())})
  hats <- reactive({hatvalues(reg1())})
  zqq <- reactive({data.frame(coeff1(),coeff2(),dt())})
  zrvf <- reactive({data.frame(coeff3(),fitv(),dt())})
  zrvo <- reactive({data.frame(coeff3(),ord(),dt())})
  zlev <- reactive({data.frame(cookd(),hats(),dt())})
  
  ###################
  # ggplots         #
  ###################
  
  pqq <- reactive({ggplot(zqq(),aes(coeff1(),coeff2())) + 
      geom_point(colour = factor(dt()),size = 2)+
      scale_colour_manual(values = c("black", "red"),guide = FALSE)+
      ggtitle("Normal Q-Q plot")+
      xlab("Theoretical Quantiles")+
      ylab("Sample Quantiles")+
      theme_bw()
  })
  prvf <- reactive({ggplot(zrvf(),aes(fitv(),coeff3()))+
      geom_point(aes(colour = factor(dt()),size = 2),size = 2)+
      scale_colour_manual(values = c("black", "red"),guide = FALSE)+
      geom_hline(yintercept = 0)+
      ggtitle("Residuals vs. Fitted Values")+
      xlab("Fitted Value")+
      ylab("Residual")+
      theme_bw()
  })
  prvo <- reactive({ggplot(zrvo(),aes(ord(),coeff3()))+
      geom_point(aes(colour = factor(dt())),size = 2)+geom_line()+
      scale_colour_manual(values = c("black", "red"),guide = FALSE)+
      geom_hline(yintercept = 0)+
      ggtitle("Residuals vs. Order")+
      xlab("Fitted Value")+
      ylab("Residual")+
      theme_bw()
  })
  peh <- reactive({
    ggplot(zrvf(),aes(coeff3(),fill = factor(dt()))) + 
      geom_histogram(bins = 2*ceiling(log2(length(coeff3())))+1,colour = "black")+
      scale_fill_manual(values = c("grey","red"),guide = FALSE)+
      xlab("Residual")+
      ylab("Count")+
      ggtitle("Histogram of Residuals")+
      theme_bw()
  })
  plev <- reactive({
    ggplot(zlev(),aes(hats(),cookd())) + 
      geom_point(aes(colour = factor(dt())),size = 2)+
      scale_colour_manual(values = c("black", "red"),guide = FALSE)+
      xlab("Leverage")+
      ylab("Cook's Distance")+
      ggtitle("Cook's Distance vs. Leverage")+
      theme_bw()
  })
  
  
  
  output$sumstat <- renderPrint({if(input$rsummary == TRUE){summary(lm(y()~x()))}})
  ############
  #scatterplot
  ############
  output$basicreg <- renderPlot({
    x <- x()
    rmod <- lm(y()~x)
    seqt <- seq(0,10,length.out = length(x()))
    pred1 <- predict(rmod, newdata=list(x=seqt), interval="predict", level=.95)[,2]
    pred2 <- predict(rmod, newdata=list(x=seqt), interval="predict", level=.95)[,3]
    conf1 <- predict(rmod, newdata=list(x=seqt), interval="confidence", level=.95)[,2]
    conf2 <- predict(rmod, newdata=list(x=seqt), interval="confidence", level=.95)[,3]
    seq2 <- rep(1,length(x()))
    
    z2 <- data.frame(x(),y(),pred1,pred2,conf1,conf2,seqt,seq2)
    
    if(input$Pred == FALSE){
      if(input$Conf == FALSE){
        p <- ggplot(z2,aes(x(),y()))
        coeff <- reg1()$coefficients
        eq <- paste0("Scatterplot with y = ", round(coeff[2],2), "*x + ", round(coeff[1],2))
        plot1<- p+geom_point(size = 2)+
          geom_abline(intercept = reg1()$coefficients[1],slope = reg1()$coefficients[2],color = "red",size = 1.2)+
          ggtitle(eq)+
          xlab("X")+
          ylab("Y")+
          theme_bw()
        plot1
      }
      else{
        p <- ggplot(z2,aes(x(),y()))
        coeff <- reg1()$coefficients
        eq <- paste0("Scatterplot with y = ", round(coeff[2],2), "*x + ", round(coeff[1],2))
        plot1<- p+geom_point(size = 2)+
          geom_abline(intercept = reg1()$coefficients[1],slope = reg1()$coefficients[2],color = "red",size = 1.2)+
          ggtitle(eq)+
          xlab("X")+
          ylab("Y")+
          geom_line(aes(x = seqt,y = conf1),linetype = "longdash",colour = "blue",size = 1.2)+
          geom_line(aes(x = seqt,y = conf2),linetype = "longdash",colour = "blue",size = 1.2)+
          scale_colour_manual(guide = FALSE) +
          scale_linetype_manual(guide = FALSE)+
          theme_bw()
        plot1
      }
    }
    else{
    if(input$Conf == FALSE){
        p <- ggplot(z2,aes(x(),y()))
        coeff <- reg1()$coefficients
        eq <- paste0("Scatterplot with y = ", round(coeff[2],2), "*x + ", round(coeff[1],2))
        plot1<- p+geom_point(size = 2)+
          geom_abline(intercept = reg1()$coefficients[1],slope = reg1()$coefficients[2],color = "red",size = 1.2)+
          ggtitle(eq)+
          xlab("X")+
          ylab("Y")+
          geom_line(aes(x = seqt,y = pred1),linetype = "longdash", colour = "red",size = 1.2) +
          geom_line(aes(x = seqt,y = pred2),linetype = "longdash",colour = "red",size = 1.2) +
          scale_colour_manual(guide = FALSE) +
          scale_linetype_manual(guide = FALSE)+
          theme_bw()
        plot1
        
      }
      else{
        p <- ggplot(z2,aes(x(),y()))
        coeff <- reg1()$coefficients
        eq <- paste0("Scatterplot with y = ", round(coeff[2],2), "*x + ", round(coeff[1],2))
        plot1<- p+geom_point(size = 2)+
          geom_abline(intercept = reg1()$coefficients[1],slope = reg1()$coefficients[2],color = "red",size = 1.2)+
          ggtitle(eq)+
          xlab("X")+
          ylab("Y")+
          geom_line(aes(x = seqt,y = conf1),linetype = "longdash",colour = "blue",size = 1.2)+
          geom_line(aes(x = seqt,y = conf2),linetype = "longdash",colour = "blue",size = 1.2)+
          geom_line(aes(x = seqt,y = pred1),linetype = "longdash", colour = "red",size = 1.2) +
          geom_line(aes(x = seqt,y = pred2),linetype = "longdash", colour = "red",size = 1.2) +
          scale_colour_manual(guide = FALSE) +
          scale_linetype_manual(guide = FALSE)+
          theme_bw()
        plot1
      }
    }
    
  })
  ###########
  #QQ plot ##
  ###########
  
  output$topright <- renderPlot({ 
    if(input$trp == "qq"){
      pqq()}
    else if(input$trp == "rvf"){
      prvf()}
    else if(input$trp == "rvo"){
      prvo()}
    else if(input$trp == "eh"){
      peh()}
    else if(input$trp == "cvl"){
      plev()}
  })
  #############
  #Resid vs fit 
  #############
  output$bottomleft <- renderPlot({
    if(input$blp == "qq"){
      pqq()}
    else if(input$blp == "rvf"){
      prvf()}
    else if(input$blp == "rvo"){
      prvo()}
    else if(input$blp == "eh"){
      peh()}
    else if(input$blp == "cvl"){
      plev()}
  })
  ###############
  #Resid vs order
  ###############
  output$bottomright <- renderPlot({
    if(input$brp == "qq"){
      pqq()}
    else if(input$brp == "rvf"){
      prvf()}
    else if(input$brp == "rvo"){
      prvo()}
    else if(input$brp == "eh"){
      peh()}
    else if(input$brp == "cvl"){
      plev()}
  })
  
  output$dtab <- renderDataTable(if(input$viewfile==TRUE){read.csv(inFile()$datapath)}
  else{NULL}
        )
  #output$test <- renderPlot({plev()})
  #output$test <- renderPrint({
  #as.vector(unlist(ximp()))
  #})
}


shinyApp(ui = ui,server = server)


