shinyUI(fluidPage(
  
  # titel panel
  titlePanel("Squirrel Dashboard"),
  
  # use sidebar layout
  sidebarLayout(
    
    # sidebar
    sidebarPanel(
      h1("Rattle Map"),
      "Display a map of rattles from behaviour and trapping records.",
      h3("Grid and year: "),
      fluidRow(
        column(6, selectInput("grid_input", NULL, grids)),
        column(6, selectInput("year_input", NULL, years))
      ),
      actionButton("submit", "Submit"),
      conditionalPanel(condition = "input.submit > 0",
        h3("Filter"),
        uiOutput("date_input"),
        downloadButton('download_data', 'Download')
      ),
      width = 3
    ),
    
    # main panel
    mainPanel(
      ggvisOutput("rattlemap_plot")
    )
  ),
  DT::dataTableOutput("rattlemap_table")
))