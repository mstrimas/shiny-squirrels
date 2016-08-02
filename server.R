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
                   separator = "-")
  })
  # loc x
  output$locx_input <- renderUI({
    if (is.data.frame(rattles()) & nrow(rattles()) > 0) {
      locs <- range(rattles()$x)
    } else {
      locs <- c(0, 22)
    }
    locs[1] <- floor(locs[1])
    locs[2] <- ceiling(locs[2])
    sliderInput("locx_input", "Loc X: ", 
                min = locs[1], max = locs[2],
                value = locs,
                step = 1)
  })
  # loc y
  output$locy_input <- renderUI({
    if (is.data.frame(rattles()) & nrow(rattles()) > 0) {
      locs <- range(rattles()$y)
    } else {
      locs <- c(0, 22)
    }
    locs[1] <- floor(locs[1])
    locs[2] <- ceiling(locs[2])
    sliderInput("locy_input", "Loc Y: ", 
                min = locs[1], max = locs[2],
                value = locs,
                step = 1)
  })
  
  # reactive expressions for filters
  # required because web page inputs not updated until after full flush
  # date
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
  # loc x
  locx_range <- reactive({
    r <- rattles()
    i <- input$locx_input
    # if new data have just been pulled reference data frame directly
    if (is.data.frame(r) & nrow(r) & fresh_data) {
      locs <- range(r$x)
      locs[1] <- floor(locs[1])
      locs[2] <- ceiling(locs[2])
    } else if (!is.null(i)) {
      locs <- i
    } else {
      locs <- NULL
    }
    return(locs)
  })
  # loc y
  locy_range <- reactive({
    r <- rattles()
    i <- input$locy_input
    # if new data have just been pulled reference data frame directly
    if (is.data.frame(r) & nrow(r) & fresh_data) {
      locs <- range(r$y)
      locs[1] <- floor(locs[1])
      locs[2] <- ceiling(locs[2])
    } else if (!is.null(i)) {
      locs <- i
    } else {
      locs <- NULL
    }
    return(locs)
  })
  
  # filtered data
  rattles_filtered <- reactive({
    # next 3 lines required to ensure reactive dependence on ranges
    dates <- date_range()
    locx <- locx_range()
    locy <- locy_range()
    if (is.data.frame(rattles()) & nrow(rattles()) > 0 & 
        !is.null(dates) & !is.null(locx) & !is.null(locy)) {
      r <- rattles() %>% 
        filter(date >= dates[1], date <= dates[2],
               x >= locx[1], x <= locx[2],
               y >= locy[1], y <= locy[2])
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
  })
  
  # data table
  output$rattlemap_table = DT::renderDataTable(
    if (!is.null(rattles_filtered())) {
      rattles_filtered() %>% select(-id)
    } else {
      NULL
    },
    server = TRUE,
    options = list(pageLength = 10, autoWidth = TRUE),
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = c(
      "Squirrel ID" = "squirrel_id",
      "Loc X" = "x",
      "Loc Y" = "y",
      "Grid" = "grid",
      "Sex" = "sex",
      "Colours" = "colours",
      "Tags" = "tags", 
      "Rattle Date" = "date",
      "Last Trapped" = "trap_date"),
    filter = "top"
  )
  
  # download
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("rattles-",
             input$grid_input, "-",
             input$year_input,
             ".csv")
    },
    content = function(file) {
      data <- rattles_filtered()
      validate(
        need(is.data.frame(data) & nrow(data) > 0, 
             "No data to download")
      )
      write_csv(data, file)
    }
  )
})