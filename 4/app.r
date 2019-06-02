library("shiny")

ui = pageWithSidebar(
  headerPanel("Aufgabe 1 - Datensatz 'titanic'"),
  sidebarPanel(width = 2,
               checkboxInput("classInputId", "class"),
               checkboxInput("sexInputId", "sex"),
               checkboxInput("ageInputId", "age"),
               checkboxInput("shadeInputId", "shade"),
               hr(),
               checkboxInput("simpsonInputId", "simpson paradox")
               ),
  mainPanel(
    width = 10,
    sidebarPanel(
      width = 10,
      fluid = TRUE,
      tabsetPanel(
        tabPanel("Mosaik", plotOutput("mosaikplot"))
      ),
      dataTableOutput("thirdclassAll"),
      dataTableOutput("crewAll"),
      hr(),
      hr(),
      dataTableOutput("thirdclassMen"),
      dataTableOutput("thirdclassWomen"),
      dataTableOutput("crewMen"),
      dataTableOutput("crewWomen")
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
                          shade = input$shadeInputId, 
                          direction = "v",
                          color = TRUE)
  return(mosaicplot)
}

createThirdclassAll = function(isShowSimpson){
  titanic = data.frame(Titanic)
  ThirdClassData = subset(titanic, Class == "3rd")
  
  return(createTable(isShowSimpson, ThirdClassData, "Third (Men & Women)"))
}

createThirdClassMen = function(isShowSimpson){
  titanic = data.frame(Titanic)
  ThirdClassData = subset(titanic, Class == "3rd" & Sex == "Male")
  
  return(createTable(isShowSimpson, ThirdClassData, "Third (Men)"))
}

createThirdClassWomen = function(isShowSimpson){
  titanic = data.frame(Titanic)
  ThirdClassData = subset(titanic, Class == "3rd" & Sex == "Female")
  
  return(createTable(isShowSimpson, ThirdClassData, "Third (Women)"))
}

createCrewAll = function(isShowSimpson){
  titanic = data.frame(Titanic)
  CrewAllData = subset(titanic, Class == "Crew")
  
  return(createTable(isShowSimpson, CrewAllData, "Crew (Men & Women)"))
}

createCrewMen = function(isShowSimpson){
  titanic = data.frame(Titanic)
  CrewAllData = subset(titanic, Class == "Crew" & Sex == "Male")
  
  return(createTable(isShowSimpson, CrewAllData, "Crew (Men)"))
}

createCrewWomen = function(isShowSimpson){
  titanic = data.frame(Titanic)
  CrewAllData = subset(titanic, Class == "Crew" & Sex == "Female")
  
  return(createTable(isShowSimpson, CrewAllData, "Crew (Women)"))
}

createTable = function(isShowSimpson, dataframe, headertitle){
  if(isShowSimpson){
    matrix = matrix(c(headertitle, 
                      getSumSurvived(dataframe),
                      getSumLost(dataframe),
                      getSumSurvived(dataframe) + getSumLost(dataframe),
                      getSurvivalRate(getSumSurvived(dataframe), (getSumSurvived(dataframe) + getSumLost(dataframe)))
    ), 
    ncol = 5, byrow = TRUE)
    colnames(matrix) <- c("Class","Saved","Lost", "Total", "Survival Rate")
    
    return(as.table(matrix))
  }
}

getSumSurvived = function(data){
  return(sum(subset(data, Survived == "Yes")$Freq))
}

getSumLost = function(data){
  return(sum(subset(data, Survived == "No")$Freq))
}

getSurvivalRate = function(survivedSum, total){
  return(survivedSum / (total / 100))
}

server = function(input, output) {
  output$mosaikplot = renderPlot(createMosaiqPlot(input))
  
  output$thirdclassAll = renderDataTable(createThirdclassAll(input$simpsonInputId))
  output$crewAll = renderDataTable(createCrewAll(input$simpsonInputId))
  output$thirdclassMen = renderDataTable(createThirdClassMen(input$simpsonInputId))
  output$thirdclassWomen = renderDataTable(createThirdClassWomen(input$simpsonInputId))
  output$crewMen = renderDataTable(createCrewMen(input$simpsonInputId))
  output$crewWomen = renderDataTable(createCrewWomen(input$simpsonInputId))
}

shinyApp(ui = ui, server = server)


