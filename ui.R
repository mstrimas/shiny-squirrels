shinyUI(navbarPage(
  
  # title
  "Squirrel Dashboard",
  
  ##########   Rattle Map   ########## 
  tabPanel("Rattle Map",
    sidebarLayout(
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
          uiOutput("locx_input"),
          uiOutput("locy_input"),
          downloadButton('download_data', 'Download')
        ),
        width = 3
      ),
      
      # main panel
      mainPanel(
        tabsetPanel(
          tabPanel("Map", ggvisOutput("rattlemap_plot")),
          tabPanel("Table", DT::dataTableOutput("rattlemap_table"))
        )
      )
    )
  ),
  
  ##########   Progress   ########## 
  tabPanel("Progress"),
  
  ##########   Error Checking   ########## 
  tabPanel("Error Checking")
))