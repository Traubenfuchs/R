library("shiny")
library("e1071")

ui = pageWithSidebar(
  headerPanel("Aufgabe 3 - LakeHuron"),
  sidebarPanel(width = 2),
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
        tabPanel("Residual Plots", plotOutput("residualPlots"))
      )
    )
  )
)

createHistogramWithBellCurve = function(){
  g = LakeHuron
  h = hist(g, breaks = 39, freq = TRUE, xlab = "Water Level")

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

createQQPlot = function() {
  plotqq = qqnorm(LakeHuron)
  plotqq = qqline(LakeHuron)
  return(plotqq)
}

createHistogram = function () {
  histogram = hist(
    LakeHuron,
    freq = FALSE,
    main = "Wasserstand",
    xlab = "Percentage"
  )
  histogram = lines(density(LakeHuron))
  return(histogram)
}

createBoxplot = function() {
  boxpl = boxplot(horizontal = TRUE, LakeHuron)
  abline(v = mean(LakeHuron))
  return(boxpl)
}

createText = function(type) {
if (type == "skewness") {
  skewness = skewness(LakeHuron)
  skewness = round(skewness, 2)
  return (paste0("skewness:    ", skewness))
}
  if (type == "mean") {
    meanVar = mean(LakeHuron)
    meanVarRound = round(meanVar, 2)
    return (paste0("Mean:    ", meanVarRound))
  }
  if (type == "trimmedMean") {
    meanVar = mean(LakeHuron, trim = 0.05)
    meanVarRound = round(meanVar, 2)
    return (paste0("Trimmed Mean:    ", meanVarRound))
  }
  if (type == "median") {
    medianVar = median(LakeHuron)
    return(paste0("Median:    ", medianVar))
  }
  if (type == "mode") {
    modeVar = getmode(LakeHuron)
    return (paste0("Mode:    ", modeVar))
  }
  if (type == "range") {
    rangeVar = range(LakeHuron)
    return(paste0("Range:    ", rangeVar[2] - rangeVar[1]))
  }
  if (type == "quartile") {
    quartileVar = fivenum(LakeHuron)
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
    quartileVar = IQR(LakeHuron)
    return(paste0("IQR (Interquartile Range): ", quartileVar))
  }

  if (type == "variance") {
    varianceVar = var(LakeHuron)
    varianceVarRound = round(varianceVar, 2)
    return (paste0("Variance:    ", varianceVarRound))
  }
  if (type == "standardDeviation") {
    standardDeviationVar = sd(LakeHuron)
    standardDeviationVarRound = round(standardDeviationVar, 2)
    return(paste0("Standard Deviation:    ", standardDeviationVarRound))
  }
}

server = function(input, output) {
  ##################################
  # text output
  output$barplotInterMean = renderText(createText("mean"))
  output$barplotInterTmean = renderText(createText("trimmedMean"))
  output$barplotInterMedian = renderText(createText("median"))
  output$barplotInterMode = renderText(createText("mode"))
  output$barplotInterRange = renderText(createText("range"))
  output$barplotInterQuartile = renderText(createText("quartile"))
  output$barplotInterQuartileAbstand = renderText(createText("quartileAbstand"))
  output$barplotInterVariance = renderText(createText("variance"))
  output$barplotInterStdDeviation = renderText(createText("standardDeviation"))
  output$barplotInterSkewness = renderText(createText("skewness"))

  ##################################
  # plot output
  output$qqPlot = renderPlot(createQQPlot())
  output$histogram = renderPlot(createHistogram())
  output$boxplot = renderPlot(createBoxplot())
  output$histogrammBellCurve = renderPlot(createHistogramWithBellCurve())

  output$residualPlots = renderPlot({
      lmResult = lm(LakeHuron ~ seq(1875,1972,length=length(LakeHuron)))
      par(mfrow = c(2, 2))
      plot(lmResult)
    }, height=500)

  #independantAttirbuteDropdown = indep
  #dependantAttirbuteDropdown = dep
  #lm(dep,indep)
  output$scatterplot2 <- renderPlot({
    plot(seq(1875,1972,length=length(LakeHuron)), LakeHuron,
         main="Scatterplot",
         xlab=paste("independant: ", "indep"),
         ylab=paste("dependant: ", "dep"),
         pch=19)

    # https://bookdown.org/paulcbauer/idv2/8-20-example-a-simple-regression-app.html
    lmResult = lm(LakeHuron ~ seq(1875,1972,length=length(LakeHuron)))
    predicted = predict(lmResult)
    residuals = residuals(lmResult)
    abline(lmResult, col="red")

    # Lowess Smoothing (dynamic line following the data)
    lines(lowess(seq(1875,1972,length=length(LakeHuron)),LakeHuron), col="blue")

  }, height=400)
}

shinyApp(ui = ui, server = server)