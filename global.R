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