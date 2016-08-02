shinyServer(function(input, output, session) {
  fresh_data <- TRUE
  
  ##########   Rattle Map   ########## 
  # data
  rattles <- eventReactive(input$submit, {
    fresh_data <<- TRUE
    kp(pool) %>%
      krsp_rattlemap(grid = input$grid_input, year = input$year_input, 
                     data = TRUE)
  })
  reverse_grid <- eventReactive(input$submit, {input$grid_input == "AG"})
  # filters for rattle data frame
  # date
  output$date_input <- renderUI({
    if (is.data.frame(rattles()) & nrow(rattles()) > 0) {
      dates <- range(rattles()$date)
    } else {
      dates <- rep(Sys.Date(), 2)
    }
    dateRangeInput("date_input", "Dates: ",
                   start = dates[1], end = dates[2],
                   min = dates[1], max = dates[2],
                   separator= "-")
  })
  # reactive expression for data range to filter
  # required because web page inputs not updated until after full flush
  date_range <- reactive({
    r <- rattles()
    i <- input$date_input
    # if new data have just been pulled reference data frame directly
    if (is.data.frame(r) & nrow(r) & fresh_data) {
      dates <- range(r$date)
    } else if (!is.null(i)) {
      dates <- i
      if (dates[1] > dates[2]) {
        message_plot("To date must be after from date.") %>%
          bind_shiny("rattlemap_plot")
        dates <- NULL
      }
    } else {
      dates <- NULL
    }
    return(dates)
  })
  # filtered data
  rattles_filtered <- reactive({
    # next line required to ensure reactive dependence on date_range()
    dates <- date_range()
    if (is.data.frame(rattles()) & nrow(rattles()) > 0 & !is.null(dates)) {
      r <- rattles() %>% 
        filter(date >= dates[1], date <= dates[2])
      if (nrow(r) == 0) {
        r <- NULL
      }
    } else {
      r <- NULL
    }
    return(r)
  })
  # plot
  observe({
    #if (!is.null(date_range())) {
      if (!is.null(rattles_filtered())) {
        rattles_filtered() %>% 
          krsp:::plot_rattles(reverse_grid()) %>% 
          bind_shiny("rattlemap_plot")
      } else {
        message_plot("No rattles found.") %>%
          bind_shiny("rattlemap_plot")
      }
      # ui now updated, data no longer fresh
      fresh_data <<- FALSE
    #}
  })
  # data table
  output$rattlemap_table = DT::renderDataTable(
    rattles_filtered(),
    server = FALSE, 
    options = list(pageLength = 10, autoWidth = TRUE),
    rownames = FALSE
  )
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("rattles-",
             input$grid_input, "-",
             input$year_input,
             ".csv")
    },
    content = function(file) {
      write_csv(rattles_filtered(), file)
    }
  )
})