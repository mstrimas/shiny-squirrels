# Shiny Squirrels

Shiny Squirrels in a dashboard for the Kluane Red Squirrel Project (KRSP) created with R and Shiny. To use this dashboard, you'll need a access to the KRSP database. To run the dashboard:

1. [Download a ZIP file](https://github.com/mstrimas/shiny-squirrels/archive/master.zip) of the repository.
2. Open the RStudio project `shiny-squirrels.Rproj`.
3. Install the latest versions of the following packages:

  ```
  install.packages("devtools")
  devtools::install_github("mstrimas/krsp")
  devtools::install_github("rstudio/shiny")
  install.packages(DBI)
  devtools::install_github(pool)
  install.packages(ggvis)
  install.packages(DT)
  devtools::install_github('htmlwidgets/sparkline')
  install.packages(readr)
  install.packages(dplyr)
  ```
4. Update `global.R` to include your credentials for connecting to the KRSP database.
5. In RStudio, click the "Run App" button with the green arrow (top-right corner of the source pane) and the dashboard will open in your browser.