# A nice shiny app
library(shiny)
app <- shinyApp(
  ui = bootstrapPage(
    selectInput("filterCols","Seleccione columnas para filtrar",choices = names(d),selected = NULL,multiple = TRUE),
    isotopeOutput("viz")
  ),
  server = function(input, output) {
    d <- read.csv(system.file("data/southAmericaCountries.csv", package = "isotope"))
    output$viz <- renderIsotope({
      filterCols <- input$filterCols
      sortCols <- NULL
      isotope(d[-1], filterCols = filterCols, sortCols = "idioma", lang = 'en')
    })
  }
)
runApp(app)

