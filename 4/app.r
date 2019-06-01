library("shiny")

ui = pageWithSidebar(
  headerPanel("Aufgabe 1 - Datensatz 'titanic'"),
  sidebarPanel(width = 2,
               checkboxInput("classInputId", "class"),
               checkboxInput("sexInputId", "sex"),
               checkboxInput("ageInputId", "age"),
               checkboxInput("shadeInputId", "shade")
               ),
  mainPanel(
    width = 10,
    sidebarPanel(
      width = 10,
      fluid = TRUE,
      tabsetPanel(
        tabPanel("Mosaik", plotOutput("mosaikplot"))
      )
    )
  )
)

createMosaiqPlot = function(input) {
  formulaString = "~ Survived "
  
  if(input$classInputId) {
    formulaString = paste(formulaString, "+ Class ", sep="")
  }
  
  if(input$sexInputId) {
    formulaString = paste(formulaString, "+ Sex ", sep="")
  }
  
  if(input$ageInputId) {
    formulaString = paste(formulaString, "+ Age ", sep="")
  }
  
  formula = as.formula(formulaString)
  mosaicplot = mosaicplot(formula, main = "Survival on the Titanic", 
                          data = Titanic,
                          pop = FALSE, 
                          shade = input$shadeInputId, 
                          direction = "v",
                          color = TRUE)
  return(mosaicplot)
}

server = function(input, output) {
  output$mosaikplot = renderPlot(createMosaiqPlot(input))
}

shinyApp(ui = ui, server = server)


