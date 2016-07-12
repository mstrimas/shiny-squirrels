library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Squirrel Rattle Locations"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("grid", "Grid:", krsp:::grid_list()),
      sliderInput("year",
                  "Number of bins:",
                  min = 1984,
                  max = krsp:::current_year(),
                  value = krsp:::current_year(),
                  step = 1)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
