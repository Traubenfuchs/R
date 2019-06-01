library("shiny")
library("e1071") 

ui = pageWithSidebar(
  headerPanel("Aufgabe 2 - Datensatz 'state.x77'"),
  sidebarPanel(width = 2,
               selectInput(
                 "independantAttirbuteDropdown",
                 "Attribute (indep)",
                 c (
                   "Population",
                   # we can use key==value for most dropdown values
                   "Income",
                   "Illiteracy",
                   "Life Exp" = "Life.Exp",
                   # for the last two we want a nice display name
                   "Murder",
                   "HS Grad" = "HS.Grad",
                   "Frost"
                 )
               ),selectInput(
                 "dependantAttirbuteDropdown",
                 "Counter Attribut (Scatterplot, dependant)",
                 c (
                   "Population",
                   # we can use key==value for most dropdown values
                   "Income",
                   "Illiteracy",
                   "Life Exp" = "Life.Exp",
                   # for the last two we want a nice display name
                   "Murder",
                   "HS Grad" = "HS.Grad",
                   "Frost"
                 )
               )),
  mainPanel(
    width = 10,
    sidebarPanel(
      width = 5,
      fluid = TRUE,
      textOutput("barplotInterMean"),
      textOutput("barplotInterTmean"),
      textOutput("barplotInterMedian"),
      textOutput("barplotInterMode"),
      textOutput("barplotInterRange"),
      textOutput("barplotInterQuartile"),
      textOutput("barplotInterQuartileAbstand"),
      textOutput("barplotInterVariance"),
      textOutput("barplotInterStdDeviation"),
      textOutput("barplotSkewness")
    ),
    sidebarPanel(
      width = 10,
      fluid = TRUE,
      tabsetPanel(
        tabPanel("Test", plotOutput("test")),
        tabPanel("QQ plot", plotOutput("qqPlot")),
        tabPanel("Histogram", plotOutput("histogram")),
        tabPanel("Histogramm Detail", plotOutput("histogrammBellCurve")),
        tabPanel("Boxplot", plotOutput("boxplot")),
        tabPanel("Scatterplot lm fancy", plotOutput("scatterplot2")),
        tabPanel("Scatterplot residual visualisation", plotOutput("scatterplot3"))
      )
    )
  )
)

getmode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

createTest = function(selectedAttribute){
  plot(get(selectedAttribute, data.frame(state.x77)))
}

createQQPlot = function(selectedAttribute) {
  plotqq = qqnorm(get(selectedAttribute, data.frame(state.x77)))
  plotqq = qqline(get(selectedAttribute, data.frame(state.x77)))
  return(plotqq)
}

createHistogram = function (selectedAttribute) {
  x = get(selectedAttribute, data.frame(state.x77))
  h = hist(
    x,
    freq = FALSE,
    main = selectedAttribute,
    xlab = "Percentage"
  )
  h = lines(density(x))
  return(h)
}

createHistogramWithBellCurve = function(selectedAttribute){
  g = get(selectedAttribute, data.frame(state.x77))
  h = hist(g, breaks = 39, freq = TRUE, xlab = "Percentage", main = selectedAttribute) 
  
  xfit = seq(min(g), max(g), length = 40) 
  yfit = dnorm(xfit, mean = mean(g), sd = sd(g)) 
  yfit = yfit * diff(h$mids[1:2]) * length(g) 
  
  lines(xfit, yfit)
  abline(v = mean(g), col = "blue")
  
  sk = ""
  
  if(skewness(g) > 0)
    sk = "rechts schief"
  else if (skewness(g) < 0)
    sk = "links schief"
  else 
    sk = "symmetrisch"
  
  legend(min(h$breaks), max(h$counts), 
         legend=c("Mean", sk),
         col=c("blue", "black"), lty=1:1, cex=0.8)
    
  return(h)
}

createBoxplot = function(selectedAttribute) {
  boxpl = boxplot(horizontal = TRUE, get(selectedAttribute, data.frame(state.x77)))
  abline(v = mean(get(selectedAttribute, data.frame(state.x77))), col="blue")
  return(boxpl)
}

createScatterPlot = function (selectedAttribute, counterAttribute) {
  plot(get(counterAttribute, data.frame(state.x77)), get(selectedAttribute, data.frame(state.x77)), xlab = counterAttribute,  ylab = selectedAttribute)
}

createText = function(type, selectedAttribute) {
  if (type == "skewness") {
    skewness = skewness(get(selectedAttribute, data.frame(state.x77)))
    skewness = round(skewness, 2)
    return (paste0("skewness:    ", skewness))
  }
  if (type == "mean") {
    meanVar = mean(get(selectedAttribute, data.frame(state.x77)))
    meanVarRound = round(meanVar, 2)
    return (paste0("Mean:    ", meanVarRound))
  }
  if (type == "trimmedMean") {
    meanVar = mean(get(selectedAttribute, data.frame(state.x77)), trim = 0.05)
    meanVarRound = round(meanVar, 2)
    return (paste0("Trimmed Mean:    ", meanVarRound))
  }
  if (type == "median") {
    medianVar = median(get(selectedAttribute, data.frame(state.x77)))
    return(paste0("Median:    ", medianVar))
  }
  if (type == "mode") {
    return (paste0("Mode: ", getmode(get(selectedAttribute, data.frame(state.x77)))))
  }
  if (type == "range") {
    rangeVar = range(get(selectedAttribute, data.frame(state.x77)))
    return(paste0("Range:    ", rangeVar[2] - rangeVar[1]))
  }
  if (type == "quartile") {
    quartileVar = fivenum(get(selectedAttribute, data.frame(state.x77)))
    return(
      paste0(
        "Min:",
        quartileVar[1],
        cat("\n"),
        " | 1.Quartil:",
        quartileVar[2],
        " | Median: " ,
        quartileVar[3],
        " | 3.Quartil: ",
        quartileVar[4],
        " | Max: ",
        quartileVar[5]
      )
    )
  }
  if (type == "quartileAbstand") {
    quartileVar = IQR(get(selectedAttribute, data.frame(state.x77)))
    return(paste0("IQR (Interquartile Range): ", quartileVar))
  }
  
  if (type == "variance") {
    varianceVar = var(get(selectedAttribute, data.frame(state.x77)))
    varianceVarRound = round(varianceVar, 2)
    return (paste0("Variance:    ", varianceVarRound))
  }
  if (type == "standardDeviation") {
    standardDeviationVar = sd(get(selectedAttribute, data.frame(state.x77)))
    standardDeviationVarRound = round(standardDeviationVar, 2)
    return(paste0("Standard Deviation:    ", standardDeviationVarRound))
  }
}

server = function(input, output) {
  ##################################
  # text output
  output$barplotInterMean = renderText(createText("mean", input$independantAttirbuteDropdown))
  output$barplotInterTmean = renderText(createText("trimmedMean", input$independantAttirbuteDropdown))
  output$barplotInterMedian = renderText(createText("median", input$independantAttirbuteDropdown))
  output$barplotInterMode = renderText(createText("mode", input$independantAttirbuteDropdown))
  output$barplotInterRange = renderText(createText("range", input$independantAttirbuteDropdown))
  output$barplotInterQuartile = renderText(createText("quartile", input$independantAttirbuteDropdown))
  output$barplotInterQuartileAbstand = renderText(createText("quartileAbstand", input$independantAttirbuteDropdown))
  output$barplotInterVariance = renderText(createText("variance", input$independantAttirbuteDropdown))
  output$barplotInterStdDeviation = renderText(createText("standardDeviation", input$independantAttirbuteDropdown))
  output$barplotSkewness = renderText(createText("skewness", input$independantAttirbuteDropdown))
  
  ##################################
  # plot output
  output$qqPlot = renderPlot(createQQPlot(input$independantAttirbuteDropdown))
  output$histogram = renderPlot(createHistogram(input$independantAttirbuteDropdown))
  output$histogrammBellCurve = renderPlot(createHistogramWithBellCurve(input$independantAttirbuteDropdown))
  output$boxplot = renderPlot(createBoxplot(input$independantAttirbuteDropdown))
 output$test = renderPlot(createTest(input$independantAttirbuteDropdown))
  
  
  #independantAttirbuteDropdown = indep
  #dependantAttirbuteDropdown = dep
  #lm(dep,indep)
  output$scatterplot2 <- renderPlot({
    plot(data.frame(state.x77)[,input$independantAttirbuteDropdown], data.frame(state.x77)[,input$dependantAttirbuteDropdown],
         main="Scatterplot",
         xlab=paste("independant: ", input$independantAttirbuteDropdown),
         ylab=paste("dependant: ", input$dependantAttirbuteDropdown),
         pch=19)
    
    # https://bookdown.org/paulcbauer/idv2/8-20-example-a-simple-regression-app.html
    lmResult = lm(data.frame(state.x77)[,input$dependantAttirbuteDropdown] ~ data.frame(state.x77)[,input$independantAttirbuteDropdown])
    predicted = predict(lmResult)
    residuals = residuals(lmResult)
    abline(lmResult, col="red")
    
    # Lowess Smoothing (dynamic line following the data)
    lines(lowess(data.frame(state.x77)[,input$independantAttirbuteDropdown],data.frame(state.x77)[,input$dependantAttirbuteDropdown]), col="blue")
    
  }, height=400)
  
  output$scatterplot3 <- renderPlot({
    lmResult = lm(data.frame(state.x77)[,input$dependantAttirbuteDropdown] ~ data.frame(state.x77)[,input$independantAttirbuteDropdown])
    #predicted = predict(lmResult)
    residuals = resid(lmResult)
    
    plot(data.frame(state.x77)[,input$independantAttirbuteDropdown], residuals, ylab="Residuals", xlab=input$independantAttirbuteDropdown)
  }, height=400)
}

shinyApp(ui = ui, server = server)