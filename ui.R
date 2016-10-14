shinyUI(navbarPage(
  
  # title
  "Squirrel Dashboard",
  
  ##########   Rattle Map   ########## 
  tabPanel("Rattle Map",
    sidebarLayout(
      sidebarPanel(
        h2("Rattle Map"),
        p("Display a map of rattles from behaviour and trapping records."),
        h5("Grid and year: "),
        fluidRow(
          column(6, selectInput("grid_input_rattle", NULL, grids)),
          column(6, selectInput("year_input_rattle", NULL, years))
        ),
        actionButton("submit_rattle", "Submit"),
        conditionalPanel(condition = "input.submit_rattle > 0",
          h3("Filter"),
          uiOutput("date_input_rattle"),
          uiOutput("locx_input_rattle"),
          uiOutput("locy_input_rattle"),
          downloadButton("download_data_rattle", "Download")
        ),
        width = 3
      ),
      
      # main panel
      mainPanel(
        tabsetPanel(
          tabPanel("Map", ggvisOutput("plot_rattle")),
          tabPanel("Table", br(), DT::dataTableOutput("table_rattle"))
        )
      )
    )
  ),
  
  ##########   Progress   ########## 
  tabPanel("Progress",
    sidebarLayout(
      sidebarPanel(
        h2("Progress"),
        p("Data collection progress for females on the given grid."),
        selectInput("grid_input_progress", "Grid:", grids),
        selectInput("year_input_progress", "Year:", years),
        actionButton("submit_progress", "Submit"),
        width = 2
      ),
      
      # main panel
      mainPanel(
        tabsetPanel(
          tabPanel("Results", DT::dataTableOutput("table_progress")),
          tabPanel("Help", progress_help)
        )
      )
    )
  ),
  
  ##########   Collars   ########## 
  tabPanel("Collars",
    sidebarLayout(
      sidebarPanel(
        h2("Collars"),
        p("Show squirrels that currently have radio collars on."),
        selectInput("grid_input_collars", "Grid:", c("All", grids)),
        selectInput("year_input_collars", "Year:", years),
        actionButton("submit_collars", "Submit"),
        width = 3
      ),
      
      # main panel
      mainPanel(
        DT::dataTableOutput("table_collars")
      )
    )
  ),
  
  ##########   Data Checking   ########## 
  tabPanel("Data Checking",
    sidebarLayout(
      sidebarPanel(
        h2("Data Checking"),
        p("Run data integrity checks on the field data. ",
          "This tool only highlights potential errors, ",
          "which will need to be manually fixed in the database."),
        selectInput("grid_input_checks", "Grid:", c("All", grids)),
        selectInput("year_input_checks", "Year:", years),
        selectInput("type_input_checks", "Check:", 
                    c("Trapping", "Nests", "Collars", "Behaviour")),
        uiOutput("description_checks"), 
        fluidRow(
          column(width = 6, actionButton("submit_checks", "Submit")),
          column(width = 6,
                 conditionalPanel(
                   condition = "input.submit_checks > 0",
                   downloadButton("download_data_checks", "Download")
                  )
          )
        ),
        width = 3
      ),
      
      # main panel
      mainPanel(
        tabsetPanel(
          tabPanel("Results", br(), DT::dataTableOutput("table_checks")),
          tabPanel("Check Descriptions", br(), 
                   p("Within each category, a variety of specific checks are ",
                     "performed as noted in the",
                     strong("check"),
                     "column. A brief description of each check is given ",
                     "below. Every check corresponds to a function in the ",
                     a("krsp", href = "https://github.com/mstrimas/krsp"),
                     " R package, and further details can be found in ",
                     "documention for these functions."),
                   DT::dataTableOutput("table_checks_descriptions"))
        )
      )
    )
  ),
  
  ##########   Top Squirrelers   ########## 
  tabPanel("Top Squirrelers",
    sidebarLayout(
      sidebarPanel(
        h2("Top Squirrelers"),
        p("Display the most productive squirrelers based ",
          "on a variety of metrics. Pick a specific year ",
          "or look at the all-time best squirrelers."),
        selectInput("year_input_top", "Year:", c("All Time", years)),
        selectInput("metric_input_top", "Metric:",
                    c("trapping", "collars", "behaviour")),
        uiOutput("description_top"),
        actionButton("submit_top", "Submit"),
        width = 3
      ),
      
      # main panel
      mainPanel(
        DT::dataTableOutput("table_top")
      )
    )
  )
))