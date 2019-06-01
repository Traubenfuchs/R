library("shiny")
library("e1071")

ui = pageWithSidebar(
  headerPanel("Aufgabe 1 - Datensatz 'swiss'"),
  sidebarPanel(width = 2,
               selectInput(
                 "independantAttributeDropdown",
                 "Attribute (indep)",
                 c (
                   "Fertility",
                   # we can use key==value for most dropdown values
                   "Agriculture",
                   "Education",
                   "Catholicism" = "Catholic",
                   # for the last two we want a nice display name
                   "Infant Mortality" = "Infant.Mortality"
                 )
               ),selectInput(
                 "dependantAttributeDropdown",
                 "Counter Attribut (Scatterplot, dependant)",
                 c (
                   "Fertility",
                   # we can use key==value for most dropdown values
                   "Agriculture",
                   "Education",
                   "Catholicism" = "Catholic",
                   # for the last two we want a nice display name
                   "Infant Mortality" = "Infant.Mortality"
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
      textOutput("barplotInterSkewness")
    ),
    sidebarPanel(
      width = 10,
      fluid = TRUE,
      tabsetPanel(
        tabPanel("QQ plot", plotOutput("qqPlot")),
        tabPanel("Histogram", plotOutput("histogram")),
        tabPanel("Histogramm Detail", plotOutput("histogrammBellCurve")),
        tabPanel("Boxplot", plotOutput("boxplot")),
        tabPanel("Scatterplot lm fancy", plotOutput("scatterplot2")),
        tabPanel("Scatterplot residual visualisation", plotOutput("scatterplot3")),
        tabPanel("Residual Plots", plotOutput("residualPlots"))
      )
    )
  )
)

createHistogramWithBellCurve = function(selectedAttribute){
  g = get(selectedAttribute, swiss)
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

getmode = function(v) {
      uniqv = unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }

createQQPlot = function(selectedAttribute) {
  plotqq = qqnorm(get(selectedAttribute, swiss))
  plotqq = qqline(get(selectedAttribute, swiss))
  return(plotqq)
}

createHistogram = function (selectedAttribute) {
  histogram = hist(
    get(selectedAttribute, swiss),
    freq = FALSE,
    main = selectedAttribute,
    xlab = "Percentage"
  )
  histogram = lines(density(get(selectedAttribute, swiss)))
  return(histogram)
}

createBoxplot = function(selectedAttribute) {
  boxpl = boxplot(horizontal = TRUE, get(selectedAttribute, swiss))
  abline(v = mean(get(selectedAttribute, swiss)))
  return(boxpl)
}

createScatterPlot = function (selectedAttribute, counterAttribute) {
  plot(get(counterAttribute, swiss), get(selectedAttribute, swiss), xlab = counterAttribute,  ylab = selectedAttribute)
}

createText = function(type, selectedAttribute) {
if (type == "skewness") {
  skewness = skewness(get(selectedAttribute, swiss))
  skewness = round(skewness, 2)
  return (paste0("skewness:    ", skewness))
}
  if (type == "mean") {
    meanVar = mean(get(selectedAttribute, swiss))
    meanVarRound = round(meanVar, 2)
    return (paste0("Mean:    ", meanVarRound))
  }
  if (type == "trimmedMean") {
    meanVar = mean(get(selectedAttribute, swiss), trim = 0.05)
    meanVarRound = round(meanVar, 2)
    return (paste0("Trimmed Mean:    ", meanVarRound))
  }
  if (type == "median") {
    medianVar = median(get(selectedAttribute, swiss))
    return(paste0("Median:    ", medianVar))
  }
  if (type == "mode") {
    modeVar = getmode(get(selectedAttribute, swiss))
    return (paste0("Mode:    ", modeVar))
  }
  if (type == "range") {
    rangeVar = range(get(selectedAttribute, swiss))
    return(paste0("Range:    ", rangeVar[2] - rangeVar[1]))
  }
  if (type == "quartile") {
    quartileVar = fivenum(get(selectedAttribute, swiss))
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
    quartileVar = IQR(get(selectedAttribute, swiss))
    return(paste0("IQR (Interquartile Range): ", quartileVar))
  }

  if (type == "variance") {
    varianceVar = var(get(selectedAttribute, swiss))
    varianceVarRound = round(varianceVar, 2)
    return (paste0("Variance:    ", varianceVarRound))
  }
  if (type == "standardDeviation") {
    standardDeviationVar = sd(get(selectedAttribute, swiss))
    standardDeviationVarRound = round(standardDeviationVar, 2)
    return(paste0("Standard Deviation:    ", standardDeviationVarRound))
  }
}

server = function(input, output) {
  ##################################
  # text output
  output$barplotInterMean = renderText(createText("mean", input$independantAttributeDropdown))
  output$barplotInterTmean = renderText(createText("trimmedMean", input$independantAttributeDropdown))
  output$barplotInterMedian = renderText(createText("median", input$independantAttributeDropdown))
  output$barplotInterMode = renderText(createText("mode", input$independantAttributeDropdown))
  output$barplotInterRange = renderText(createText("range", input$independantAttributeDropdown))
  output$barplotInterQuartile = renderText(createText("quartile", input$independantAttributeDropdown))
  output$barplotInterQuartileAbstand = renderText(createText("quartileAbstand", input$independantAttributeDropdown))
  output$barplotInterVariance = renderText(createText("variance", input$independantAttributeDropdown))
  output$barplotInterStdDeviation = renderText(createText("standardDeviation", input$independantAttributeDropdown))
  output$barplotInterSkewness = renderText(createText("skewness", input$independantAttributeDropdown))

  ##################################
  # plot output
  output$qqPlot = renderPlot(createQQPlot(input$independantAttributeDropdown))
  output$histogram = renderPlot(createHistogram(input$independantAttributeDropdown))
  output$boxplot = renderPlot(createBoxplot(input$independantAttributeDropdown))
  output$histogrammBellCurve = renderPlot(createHistogramWithBellCurve(input$independantAttributeDropdown))

  output$residualPlots = renderPlot({
    lmResult = lm(swiss[,input$dependantAttributeDropdown] ~ swiss[,input$independantAttributeDropdown])
    par(mfrow = c(2, 2))
    plot(lmResult)
  }, height=500)

  #independantAttributeDropdown = indep
  #dependantAttributeDropdown = dep
  #lm(dep,indep)
  output$scatterplot2 <- renderPlot({
    plot(swiss[,input$independantAttributeDropdown], swiss[,input$dependantAttributeDropdown],
         main="Scatterplot",
         xlab=paste("independant: ", input$independantAttributeDropdown),
         ylab=paste("dependant: ", input$dependantAttributeDropdown),
         pch=19)

    # https://bookdown.org/paulcbauer/idv2/8-20-example-a-simple-regression-app.html
    lmResult = lm(swiss[,input$dependantAttributeDropdown] ~ swiss[,input$independantAttributeDropdown])

    predicted = predict(lmResult)
    residuals = residuals(lmResult)
    abline(lmResult, col="red")

    # Lowess Smoothing (dynamic line following the data)
    lines(lowess(swiss[,input$independantAttributeDropdown],swiss[,input$dependantAttributeDropdown]), col="blue")

  }, height=400)

  output$scatterplot3 <- renderPlot({
    lmResult = lm(swiss[,input$dependantAttributeDropdown] ~ swiss[,input$independantAttributeDropdown])
    #predicted = predict(lmResult)
    residuals = resid(lmResult)

    plot(swiss[,input$independantAttributeDropdown], residuals, ylab="Residuals", xlab=input$independantAttributeDropdown)
  }, height=400)
}

shinyApp(ui = ui, server = server)
