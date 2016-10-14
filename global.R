library(shiny)
library(DBI)
library(pool)
library(krsp)
library(ggvis)
library(DT)
library(sparkline)
library(readr)
library(dplyr)

pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "krsp",
  host = "localhost",
  username = "root",
  password = ""
)
# shortcut for krsp_pool
kp <- krsp_pool

# list of grids in database
grids <- kp(pool) %>% 
  krsp:::grid_list() %>% 
  sort()
years <- kp(pool) %>% 
  krsp:::year_list() %>% 
  sort(decreasing = TRUE)

# ggvis plot for no data
message_plot <- function(message = "No data found.") {
  p <- data.frame(x = 0, y = 0.8, m = message) %>% 
    ggvis(~x, ~y) %>% 
    layer_text(text := ~m, fontSize := 30, font := "Helvetica Neue") %>% 
    scale_numeric("y", domain = c(0, 1)) %>% 
    hide_axis("x") %>% 
    hide_axis("y")
  return(p)
}

# check query descriptions
check_descriptions <- read_csv("check-descriptions.csv")

# progress help
progress_help <- div(
  p("This tool provides an overview of data collection progress for ",
    "the season. All adult females caught in the given year are inlcuded ",
    "provided their most recent trapping record has fate 1-3."),
  p("The first three columns identify the squirrel by squirrel ID, tags, ",
    "colours, and location, respectively, all taken from the most recent ",
    "trapping record."),
  p("The ", strong("Litter"), " column identifies which litter number the ",
    "row applies to. Each litter a squirrel has will give rise to a ",
    "unique row in the table. For example, if a squirrel has given birth ",
    "to their second litter, they will have to rows, one with Litter = 1 ",
    "and another with Litter = 2. If a squirrel hasn't yet given birth ",
    "a litter this year, they will have a single row with Litter = 0."),
  p("The ", strong("nest status"), "of a female is based on the most recent ",
    "trapping record and the litter table. If the most recent trapping record ",
    "is dated prior to the date the litter table was updated it is ignored, ",
    "and only the litter table is used. If nipple condition is 5, the status is ",
    "LL (Lost Litter). Otherwise, status is Parturition, N1, or Completed if fieldBDate, date1, or tagDt fields
    in litter table are filled in, respectively. Finally, if litter table dates
    are empty, rep_con field in trapping record is used and status is P0, P1, P2,
    or P3 is assigned for rep_con = 1, 2, 3, 4, respectively.")
)