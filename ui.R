library(shiny)


shinyUI(
  fluidPage(theme = 'cosmo.css',
    titlePanel('Cuantiles de ls distribución Ji-cuadrada'),
    sidebarLayout(
      sidebarPanel(
        
        sliderInput('gdl', 'Grados de libertad:', 
                    min = 5,
                    max = 250,
                    value = 20,
                    step = 1),
        
        sliderInput('alfa', 'Nivel de significancia:', 
                    min = 0.01,
                    max = 0.20,
                    value = 0.05,
                    step = 0.01),
        
        uiOutput('prob')
       
        ),
      mainPanel(
        plotOutput('graf'),
        tableOutput('quants')
      )
    )
  )
)