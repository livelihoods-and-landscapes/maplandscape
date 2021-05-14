#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import leaflet
#' @import leaflet.extras
#' @importFrom dplyr select_if
#'
#' @noRd
#'

options(shiny.maxRequestSize = 5000 * 1024^2)
# library(leaflet.extras)

app_server <- function(input, output, session) {
  # move to data tab when action button pressed
  observeEvent(input$enter, {
    updateNavbarPage(session, "navbar", selected = "Data")
  })

  # Data Sync ---------------------------------------------------------------

  # sync forms with database / template
  sync_waiter <- waiter::Waiter$new(
    html = sync_screen,
    color = "rgba(44,62,80,.6)"
  )

  # select template db to sync forms to
  template <- mod_get_layers_server(id = "template_db")

  # forms to sync to template db
  forms <- mod_get_layers_server(id = "forms_db")

  # returns 4 element list
  # element 1 is file name and path to temporary geopackage
  # element 2 is date time string for creation of temporary geopackage
  # element 3 is a data frame in the same format as returned by fileUpload
  # element 4 is a log file
  gpkg_path <- reactive({
    req(template(), forms())

    sync_waiter$show()
    gpkg_path <- sync_forms(template = template(), forms = forms())
    sync_waiter$hide()
    gpkg_path
  })

  # download raw synced data as a zip file
  output$download_sync_forms <- downloadHandler(
    filename = function() {
      req(gpkg_path()[[1]])

      paste("synced_forms_", gpkg_path()[[2]], ".zip", sep = "")
    },
    content = function(file) {
      req(gpkg_path()[[1]])

      zip(
        zipfile = file,
        files = c(gpkg_path()[[1]], gpkg_path()[[4]]),
        flags = "-r9Xj"
      )
    },
    contentType = "application/zip"
  )

  # sync forms modal
  observeEvent(input$sync_forms, {
    showModal(
      modalDialog(
        tags$h4("Template or central database"),
        mod_get_layers_ui(
          id = "template_db",
          label = "Select template .gpkg",
          multiple = FALSE,
          accept = c(".gpkg")
        ),
        tags$h4("Completed forms"),
        mod_get_layers_ui(
          id = "forms_db",
          label = "Select forms .gpkg",
          multiple = TRUE,
          accept = c(".gpkg")
        ),
        hr(),
        checkboxInput("add_synced_forms", label = "add synced forms to active layer", value = TRUE),
        downloadButton("download_sync_forms", "Download"),
        hr(),
        modalButton("Go to app"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  # Data Upload -------------------------------------------------------------

  # Upload data and select active layer
  # reactiveValues object to hold dataframe of layers a user can select as the active layer
  data_file <-
    reactiveValues(
      data_file = data.frame(),
      map_drawn = 0,
      joined_df = list(),
      edit_data_file = data.frame(),
      tmp_edits = data.frame(),
      edit_log = NULL,
      flush_deletes = 1,
      flush_edits = 1,
      flush_geometry_edits = 1,
      event_tmp = NULL,
      map_edits_zoom = 0
    )

  # add synced files to app
  sync_file <- reactive({
    req(gpkg_path()[[1]], input$add_synced_forms)

    sync_file <- gpkg_path()[[3]]
    sync_file
  })

  # update reactiveValues object holding dataframe of layers a user can select as active layer
  observe({
    req(sync_file())

    sync_file <- isolate(sync_file())
    isolate({
      df <- dplyr::bind_rows(data_file$data_file, sync_file)
      # unique number id next to each layer to catch uploads of tables with same name
      rows <- nrow(df)
      row_idx <- 1:rows
      df$layer_disp_name_idx <-
        paste0(df$layer_disp_name, "_", row_idx, sep = "")
      data_file$data_file <- df
    })
  })

  # user uploaded files
  # return table of files and file paths of data loaded to the server
  upload_file <- mod_get_layers_server(id = "qfield_data")

  # update reactiveValues object holding dataframe of layers a user can select as active layer
  observe({
    req(upload_file())

    upload_file <- isolate(upload_file())
    isolate({
      df <- dplyr::bind_rows(data_file$data_file, upload_file)
      # unique number id next to each layer to catch uploads of tables with same name
      rows <- nrow(df)
      row_idx <- 1:rows
      df$layer_disp_name_idx <-
        paste0(df$layer_disp_name, "_", row_idx, sep = "")
      data_file$data_file <- df
    })
  })

  # select one table as active layer from files loaded to the server
  observe({
    df <- data_file$data_file
    joined_df <- data_file$joined_df
    nm_jdf <- names(joined_df)
    choices <- unique(df$layer_disp_name_idx)
    choices <- c(choices, nm_jdf)
    updateSelectInput(session, "active_layer", choices = choices)
  })

  # active df - use this df for summarising and generating raw tables for display
  active_df <- reactive({
    req(input$active_layer)

    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))

    if (any(jdf == input$active_layer)) {
      active_df <- data_file$joined_df[[input$active_layer]]
    } else {
      active_df <- read_tables(df, input$active_layer)
    }

    active_df
  })

  # render active df as raw data table
  mod_render_dt_server(id = "data_raw", dt = active_df, editable = FALSE)

  # Summary Tables ----------------------------------------------------------

  # Select input for grouping and summarising variables
  grouping_vars <-
    mod_multiple_input_server(id = "grouping_var", m_df = active_df)

  # filter out selected grouping variables in list of variables which can be summarised
  s_active_df <- reactive({
    req(active_df(), grouping_vars())

    tmp_df <- active_df() %>%
      select_if(is.numeric)
    choices <- names(tmp_df)
    s_intersect <- intersect(choices, grouping_vars())
    choices <- choices[!choices %in% s_intersect]

    choices
  })

  summarising_vars <-
    mod_multiple_input_server(id = "summarising_var", m_df = s_active_df)

  # perform group by and summarise operation
  summarised_df <- reactive({
    req(active_df())

    summarised_df <-
      group_by_summarise(active_df(), grouping_vars(), summarising_vars())

    summarised_df
  })

  # render summarised_df as data table
  mod_render_dt_server(id = "data_summary", dt = summarised_df, editable = FALSE)

  # Joining Tables ----------------------------------------------------------

  observe({
    df <- data_file$data_file
    joined_df <- data_file$joined_df
    choices <- unique(df$layer_disp_name_idx)
    nm_jdf <- names(joined_df)
    choices <- c(choices, nm_jdf)
    data_file$table_left <- choices

    updateSelectInput(session, "table_left", choices = choices)
  })

  left_df <- reactive({
    req(input$table_left)

    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))

    if (any(jdf == input$table_left)) {
      left_df <- isolate(data_file$joined_df[[input$table_left]])
    } else {
      left_df <- read_tables(df, input$table_left)
    }

    left_df
  })

  observe({
    df <- data_file$table_left

    choices <- unique(df)
    choices <- choices[choices != input$table_left]

    updateSelectInput(session, "table_right", choices = choices)
  })

  right_df <- reactive({
    req(input$table_right)

    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))

    if (any(jdf == input$table_right)) {
      right_df <- isolate(data_file$joined_df[[input$table_right]])
    } else {
      right_df <- read_tables(df, input$table_right)
    }

    right_df
  })

  # update select input for table left primary key
  p_key <-
    mod_multiple_input_server(id = "joining_p_key_left", m_df = left_df)

  # update select input for table right foreign key
  f_key <-
    mod_multiple_input_server(id = "joining_f_key_right", m_df = right_df)

  join_waiter <- waiter::Waiter$new(
    html = join_screen,
    color = "rgba(44,62,80,.6)"
  )

  # join left table to right table
  observeEvent(input$table_join_button, {
    req(left_df(), right_df(), input$key_join_type)

    join_waiter$show()
    if (input$key_join_type == "col_inner" |
      input$key_join_type == "col_left") {
      joined_table <-
        join_tables(
          left_df(),
          right_df(),
          input$key_join_type,
          p_key(),
          f_key()
        )
    }
    join_waiter$hide()

    data_file$joined_df[[input$join_tbl_name]] <- joined_table
  })

  # select tables for spatial joins
  observe({
    df <- data_file$data_file
    joined_df <- data_file$joined_df
    choices <- unique(df$layer_disp_name_idx)
    nm_jdf <- names(joined_df)
    choices <- c(choices, nm_jdf)
    data_file$spatial_table_left <- choices

    updateSelectInput(session, "spatial_table_left", choices = choices)
  })

  spatial_left_df <- reactive({
    req(input$spatial_table_left)

    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))

    if (any(jdf == input$spatial_table_left)) {
      left_df <- isolate(data_file$joined_df[[input$spatial_table_left]])
    } else {
      left_df <- read_tables(df, input$spatial_table_left)
    }

    shinyFeedback::feedbackWarning(
      "spatial_table_left",
      !("sf" %in% class(left_df)),
      "Not a spatial layer"
    )

    left_df
  })

  observe({
    req(input$spatial_table_left)
    df <- data_file$spatial_table_left

    choices <- unique(df)
    choices <- choices[choices != input$spatial_table_left]

    updateSelectInput(session, "spatial_table_right", choices = choices)
  })

  spatial_right_df <- reactive({
    req(input$spatial_table_right)

    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))

    if (any(jdf == input$spatial_table_right)) {
      right_df <-
        isolate(data_file$joined_df[[input$spatial_table_right]])
    } else {
      right_df <- read_tables(df, input$spatial_table_right)
    }

    shinyFeedback::feedbackWarning(
      "spatial_table_right",
      !("sf" %in% class(right_df)),
      "Not a spatial layer"
    )

    right_df
  })

  spatial_join_waiter <- waiter::Waiter$new(
    html = join_screen,
    color = "rgba(44,62,80,.6)"
  )

  # join left table to right table
  observeEvent(input$spatial_join_button, {
    req(
      spatial_left_df(),
      spatial_right_df(),
      "sf" %in% class(spatial_left_df()),
      "sf" %in% class(spatial_right_df()),
      input$spatial_join_type
    )

    spatial_join_waiter$show()
    if ("sf" %in% class(spatial_left_df()) &
      "sf" %in% class(spatial_right_df()) &
      input$spatial_join_type == "spatial_inner" |
      input$spatial_join_type == "spatial_left") {
      joined_table <-
        spatial_join_tables(
          spatial_left_df(),
          spatial_right_df(),
          input$spatial_join_type
        )
    }
    spatial_join_waiter$hide()

    data_file$joined_df[[input$spjoin_tbl_name]] <- joined_table
  })

  # Filter Rows -------------------------------------------------------------

  # filter modal
  observeEvent(input$filter, {
    showModal(
      modalDialog(
        tags$h4("Filter Options"),
        textInput(inputId = "filter_conditions", label = "Conditions to filter rows"),
        textInput(
          inputId = "filter_tbl_name",
          "Table name",
          value = "",
          placeholder = "enter table name for output"
        ),
        tags$p(
          "DEMO SNIPPET:"
        ),
        tags$code(
          "crop == 'dalo'"
        ),
        tags$p(
          "Filter conditions must be specified using dplyr syntax. Some tips:"
        ),
        tags$ul(
          tags$li("Quotes for strings - \"string\""),
          tags$li("Escape apostrophes within strings - \"vava\\'u\""),
          tags$li("Specify column names without quotes"),
          tags$li("== - equal to"),
          tags$li("!= - not equal to"),
          tags$li("<, >, <=, >= - greater than / less than comparisons"),
          tags$li("& - and"),
          tags$li("| - or")
        ),
        tags$p("Example: crop_number > 25"),
        tags$p("Example: island == \"vava\'u\""),
        actionButton("execute_filter", "Filter"),
        modalButton("close"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })


  # select tables for row filtering
  observe({
    df <- data_file$data_file
    joined_df <- data_file$joined_df
    choices <- unique(df$layer_disp_name_idx)
    nm_jdf <- names(joined_df)
    choices <- c(choices, nm_jdf)

    updateSelectInput(session, "table_filter", choices = choices)
  })


  filter_df <- reactive({
    req(input$table_filter)

    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))

    if (any(jdf == input$table_filter)) {
      filter_df <- isolate(data_file$joined_df[[input$table_filter]])
    } else {
      filter_df <- read_tables(df, input$table_filter)
    }

    filter_df
  })

  # execute filter and add filtered table to active layers
  observeEvent(input$execute_filter, {
    req(filter_df())
    req(input$filter_conditions)

    filter_df <- isolate(filter_df())

    filter_expr <- tryCatch(
      error = function(cnd) {
        NULL
      },
      {
        filter_expr <-
          call2(
            dplyr::filter,
            rlang::parse_expr("filter_df"),
            rlang::parse_expr(input$filter_conditions)
          )
      }
    )

    filter_out <- tryCatch(
      error = function(cnd) {
        NULL
      },
      {
        filter_out <- eval(filter_expr)
      }
    )

    if (length(filter_out) > 0) {
      data_file$joined_df[[input$filter_tbl_name]] <- filter_out
    }
  })

  # Add Columns -------------------------------------------------------------

  # add column modal
  observeEvent(input$add_column, {
    showModal(
      modalDialog(
        tags$h4("Add New Column"),
        textInput(inputId = "col_name", label = "New column name"),
        textInput(inputId = "mutate_conditions", label = "Function to add new column"),
        tags$p(
          "DEMO SNIPPET:"
        ),
        tags$code(
          "area.x * (area.y / 100)"
        ),
        tags$p("Function to add new column must use dplyr syntax."),
        tags$p("Example: acres * 4046.86"),
        tags$p("Example: tree_number > 0"),
        actionButton("execute_mutate", "Create column"),
        modalButton("close"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  # select table to add column to
  observe({
    df <- data_file$data_file
    joined_df <- data_file$joined_df
    choices <- unique(df$layer_disp_name_idx)
    nm_jdf <- names(joined_df)
    choices <- c(choices, nm_jdf)

    updateSelectInput(session, "table_mutate", choices = choices)
  })

  mutate_df <- reactive({
    req(input$table_mutate)

    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))

    if (any(jdf == input$table_mutate)) {
      mutate_df <- isolate(data_file$joined_df[[input$table_mutate]])
    } else {
      mutate_df <- read_tables(df, input$table_mutate)
    }

    mutate_df
  })

  # execute mutate and add column to selected table
  observeEvent(input$execute_mutate, {
    req(mutate_df())
    req(input$mutate_conditions)
    mutate_df <- isolate(mutate_df())
    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))
    col_name <- input$col_name

    mutate_expr <- tryCatch(
      error = function(cnd) {
        NULL
      },
      {
        mutate_expr <-
          call2(
            dplyr::mutate,
            rlang::parse_expr("mutate_df"),
            !!col_name := parse_expr(input$mutate_conditions)
          )
      }
    )

    mutate_out <- tryCatch(
      error = function(cnd) {
        NULL
      },
      {
        mutate_out <- eval(mutate_expr)
      }
    )

    if (length(mutate_out) > 0) {
      if (any(jdf == input$table_mutate)) {
        data_file$joined_df[[input$table_mutate]] <- mutate_out
      } else {
        tryCatch(
          error = function(cnd) {
            print("problem")
          },
          {
            a_lyr <- df %>%
              dplyr::filter(layer_disp_name_idx == input$table_mutate)
            layer <- a_lyr$layers
            sf::st_write(
              mutate_out,
              dsn = a_lyr$file_path,
              layer = layer,
              append = FALSE
            )
          }
        )
      }
    }
  })

  # Data Download -----------------------------------------------------------

  # Date stamp for downloading files
  dt <- reactive({
    d <- Sys.time()
    d <- stringr::str_replace_all(d, ":", "-")
    d <- stringr::str_replace(d, " ", "-")
    d
  })

  # download raw data
  output$download_data_raw <- downloadHandler(
    filename = function() {
      paste("raw_data_", dt(), ".csv", sep = "")
    },
    content = function(file) {
      req(active_df())

      readr::write_csv(active_df(), file)
    }
  )

  # download summarised data
  output$download_data_summarised <- downloadHandler(
    filename = function() {
      paste("summarised_data_", dt(), ".csv", sep = "")
    },
    content = function(file) {
      req(summarised_df())

      readr::write_csv(summarised_df(), file)
    }
  )

  # Web Map -----------------------------------------------------------------

  # Map options
  # update select input for mapping layer
  observe({
    df <- data_file$data_file
    joined_df <- data_file$joined_df
    nm_jdf <- names(joined_df)
    choices <- unique(df$layer_disp_name_idx)
    choices <- c(choices, nm_jdf)
    updateSelectInput(session, "map_active_layer", choices = choices)
  })

  # active df - use this df for rendering on web map
  map_active_df <- reactive({
    req(input$map_active_layer)

    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))

    if (any(jdf == input$map_active_layer)) {
      map_active_df <-
        isolate(data_file$joined_df[[input$map_active_layer]])
    } else {
      map_active_df <- read_tables(df, input$map_active_layer)
    }

    if (nrow(map_active_df) > 0) {
      map_active_df$layer_id <- as.character(1:nrow(map_active_df))
    }

    if (nrow(map_active_df) > 5000) {
      map_active_df <- map_active_df[1:5000, ]
      id <-
        showNotification(
          "Only drawing first 5000 features!",
          duration = 5,
          type = c("warning")
        )
    }

    # show warning if map active layer has no records
    shinyFeedback::feedbackWarning(
      "map_active_layer",
      !(nrow(map_active_df) > 0),
      "Not updating options - no records in selected table"
    )

    # show warning if map active layer is not spatial (class sf)
    shinyFeedback::feedbackWarning(
      "map_active_layer",
      !("sf" %in% class(map_active_df)),
      "Not updating options - not a spatial layer"
    )

    # don't update options if selected layer has no records
    req(nrow(map_active_df) > 0, "sf" %in% class(map_active_df))

    map_active_df
  })

  map_var <-
    mod_single_input_server(id = "map_var", s_df = map_active_df)

  label_vars <-
    mod_multiple_input_server(id = "label_vars", m_df = map_active_df)

  # Create web map
  output$web_map <- leaflet::renderLeaflet({
    base_map <- leaflet::leaflet() %>%
      leaflet::addTiles(group = "OSM (default)") %>%
      leaflet::addProviderTiles(
        providers$Esri.WorldImagery,
        options = providerTileOptions(maxZoom = 17),
        group = "ESRI Satellite"
      ) %>%
      leaflet::setView(0, 0, 3) %>%
      leaflet::addLayersControl(
        baseGroups = c("OSM (default)", "ESRI Satellite"),
        options = leaflet::layersControlOptions(collapsed = FALSE),
        position = c("bottomright")
      ) %>%
      leaflet::addMeasure(
        position = "bottomright",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479"
      )

    base_map
  })

  map_waiter <- waiter::Waiter$new(
    html = map_screen,
    color = "rgba(44,62,80,.6)"
  )

  # add spatial data to map
  observeEvent(input$create_map, {
    req(map_active_df())

    # map_active_df <- isolate(map_active_df())

    if ("sf" %in% class(map_active_df()) &
      is.atomic(map_active_df()[[map_var()]]) &
      nrow(map_active_df()) > 0) {
      data_file$map_drawn <- 1
      print(data_file$map_drawn)
      add_layers_leaflet(
        map_object = "web_map",
        map_active_df = map_active_df(),
        map_var = map_var(),
        map_colour = input$map_colour,
        opacity = input$opacity,
        map_line_width = input$map_line_width,
        map_line_colour = input$map_line_colour,
        waiter = map_waiter
      )
    }
  })

  # update opacity
  observeEvent(input$opacity, {
    req(map_active_df())

    if (data_file$map_drawn == 1) {
      if ("sf" %in% class(map_active_df()) &
        is.atomic(map_active_df()[[map_var()]]) &
        nrow(map_active_df()) > 0) {
        add_layers_leaflet_no_zoom(
          map_object = "web_map",
          map_active_df = map_active_df(),
          map_var = map_var(),
          map_colour = input$map_colour,
          opacity = input$opacity,
          map_line_width = input$map_line_width,
          map_line_colour = input$map_line_colour,
          waiter = map_waiter
        )
      }
    }
  })

  # update colour
  observeEvent(input$map_colour, {
    req(map_active_df())

    if (data_file$map_drawn == 1) {
      if ("sf" %in% class(map_active_df()) &
        is.atomic(map_active_df()[[map_var()]]) &
        nrow(map_active_df()) > 0) {
        add_layers_leaflet_no_zoom(
          map_object = "web_map",
          map_active_df = map_active_df(),
          map_var = map_var(),
          map_colour = input$map_colour,
          opacity = input$opacity,
          map_line_width = input$map_line_width,
          map_line_colour = input$map_line_colour,
          waiter = map_waiter
        )
      }
    }
  })

  # add popup labels
  observe({
    leaflet::leafletProxy("web_map") %>% clearPopups()

    # capture click events
    # event_shape captures a user click on a shape object
    # event_marker captures a user click on a marker object
    event_shape <- input$web_map_shape_click
    event_marker <- input$web_map_marker_click

    # if a user has not clicked on a marker or object leave event as null if a
    # user has clicked on a shape or marker update event and pass it into
    # fct_add_popups to create popup for clicked object
    event <- NULL

    if (!is.null(event_shape)) {
      event <- event_shape
    }

    if (!is.null(event_marker)) {
      event <- event_marker
    }

    if (is.null(event)) {
      return()
    }

    isolate({
      req(label_vars())

      content <-
        add_popups(
          in_df = map_active_df,
          layer_id = event$id,
          label_vars = label_vars
        )
      print(content)

      leaflet::leafletProxy("web_map") %>%
        leaflet::addPopups(event$lng, event$lat, content, layerId = event$id)
    })
  })

  # add legend on top of leaflet object
  observe({
    req(map_active_df())

    if ("sf" %in% class(map_active_df()) &
      is.atomic(map_active_df()[[map_var()]]) &
      nrow(map_active_df()) > 0) {
      # make map active layer epsg 4326
      # make this an if statement
      map_df <- map_active_df() %>%
        sf::st_transform(4326)

      bbox <- sf::st_bbox(map_df) %>%
        as.vector()

      if (class(map_df[[map_var()]]) != "numeric" &
        class(map_df[[map_var()]]) != "integer") {
        pal <- leaflet::colorFactor(input$map_colour, map_df[[map_var()]])
      } else {
        pal <- leaflet::colorNumeric(input$map_colour, map_df[[map_var()]])
      }

      if (input$legend == TRUE) {
        leaflet::leafletProxy("web_map") %>%
          leaflet::clearControls() %>%
          leaflet::addLegend(
            pal = pal,
            values = map_df[[map_var()]],
            position = "topright",
            title = input$map_legend_title
          )
      } else {
        leaflet::leafletProxy("web_map") %>%
          leaflet::clearControls()
      }
    }
  })


  # Charts ------------------------------------------------------------------

  resize_waiter <- waiter::Waiter$new(
    html = resize_screen,
    color = "rgba(44,62,80,.6)"
  )

  # Map options
  # update select input for mapping layer
  observe({
    df <- data_file$data_file
    joined_df <- data_file$joined_df
    nm_jdf <- names(joined_df)
    choices <- unique(df$layer_disp_name_idx)
    choices <- c(choices, nm_jdf)
    updateSelectInput(session, "chart_active_layer", choices = choices)
  })

  # active df - use this df for rendering chart
  chart_active_df <- reactive({
    req(input$chart_active_layer)

    df <- isolate(data_file$data_file)
    jdf <- isolate(names(data_file$joined_df))

    if (any(jdf == input$chart_active_layer)) {
      chart_active_df <-
        isolate(data_file$joined_df[[input$chart_active_layer]])
    } else {
      chart_active_df <- read_tables(df, input$chart_active_layer)
    }

    head(chart_active_df)
    chart_active_df
  })

  # histogram variable selection
  hist_choices <- reactive({
    req(chart_active_df())

    tmp_df <- chart_active_df() %>%
      select_if(is.numeric)
    choices <- names(tmp_df)
    print(choices)

    choices
  })

  hist_x_axis_vars <-
    mod_single_input_server(id = "hist_x_axis_var", s_df = hist_choices)

  # scatter plot variable selection
  scatter_choices <- reactive({
    req(chart_active_df())

    tmp_df <- chart_active_df() %>%
      select_if(is.numeric)
    choices <- names(tmp_df)
    print(choices)

    choices
  })

  scatter_x_axis_vars <-
    mod_single_input_server(id = "scatter_x_axis_var", s_df = scatter_choices)

  scatter_y_axis_vars <-
    mod_single_input_server(id = "scatter_y_axis_var", s_df = scatter_choices)

  # bar plot variables
  col_grouping_var <-
    mod_single_input_server(id = "col_grouping_var", s_df = chart_active_df)

  # filter out selected grouping variables in list of variables which can be summarised
  col_active_df <- reactive({
    req(chart_active_df(), col_grouping_var())

    tmp_df <- chart_active_df() %>%
      select_if(is.numeric)
    choices <- names(tmp_df)
    s_intersect <- intersect(choices, col_grouping_var())
    choices <- choices[!choices %in% s_intersect]

    choices
  })

  col_summarising_var <-
    mod_single_input_server(id = "col_summarising_var", s_df = col_active_df)

  # perform group by and summarise operation for bar plot
  col_summarised_df <- reactive({
    req(chart_active_df())
    chart_data()

    col_summarising_var <- isolate(col_summarising_var())
    col_grouping_var <- isolate(col_grouping_var())

    if (input$bar_plot_type == "count_records") {
      col_summarising_var <- NULL
    }

    col_group_df <-
      group_by_summarise(chart_active_df(), col_grouping_var, col_summarising_var)

    col_group_df
  })

  # make chart take reactive dependency on action button
  chart_data <- eventReactive(input$create_chart, {
    print("draw chart")
  })

  output$chart <- renderPlot(
    {
      chart_data()

      lab_font_size <- isolate(input$lab_font)
      axis_font_size <- isolate(input$axis_font)
      x_lab <- isolate(input$x_axis_label)
      y_lab <- isolate(input$y_axis_label)
      chart_type <- isolate(input$plotType)
      bar_plot_type <- isolate(input$bar_plot_type)

      if (chart_type == "histogram") {
        binwidth <- isolate(input$binwidth)
        hist_x_var <- isolate(hist_x_axis_vars())

        chart <-
          ggplot2::ggplot(isolate(chart_active_df()), ggplot2::aes(.data[[hist_x_var]])) +
          ggplot2::geom_histogram(
            binwidth = binwidth,
            color = "#2c3e50",
            fill = "#2c3e50"
          ) +
          ggplot2::xlab(x_lab) +
          ggplot2::ylab(y_lab) +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = NA, colour = NA),
            panel.background = ggplot2::element_rect(fill = NA, colour = "#2c3e50"),
            axis.text.x = ggplot2::element_text(
              angle = -45,
              vjust = 1,
              hjust = 0,
              size = axis_font_size
            ),
            axis.text.y = ggplot2::element_text(size = axis_font_size),
            axis.title.x = ggplot2::element_text(size = lab_font_size),
            axis.title.y = ggplot2::element_text(size = lab_font_size)
          )
      } else if (chart_type == "scatter") {
        scatter_x_var <- isolate(scatter_x_axis_vars())
        scatter_y_var <- isolate(scatter_y_axis_vars())
        point <- isolate(input$scatter_point_size)

        chart <-
          ggplot2::ggplot(
            isolate(chart_active_df()),
            ggplot2::aes(.data[[scatter_x_var]], .data[[scatter_y_var]])
          ) +
          ggplot2::geom_point(color = "#2c3e50", size = point) +
          ggplot2::xlab(x_lab) +
          ggplot2::ylab(y_lab) +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = NA, colour = NA),
            panel.background = ggplot2::element_rect(fill = NA, colour = "#2c3e50"),
            axis.text.x = ggplot2::element_text(
              angle = -45,
              vjust = 1,
              hjust = 0,
              size = axis_font_size
            ),
            axis.text.y = ggplot2::element_text(size = axis_font_size),
            axis.title.x = ggplot2::element_text(size = lab_font_size),
            axis.title.y = ggplot2::element_text(size = lab_font_size)
          )
      } else if (chart_type == "bar plot") {
        bar_x_var <- isolate(col_summarised_df()[, 1])

        if (bar_plot_type == "count_records") {
          bar_y_var <- isolate(col_summarised_df()[, 2])
        } else if (bar_plot_type == "sum_values") {
          bar_y_var <- isolate(col_summarised_df()[, 3])
        } else if (bar_plot_type == "mean") {
          bar_y_var <- isolate(col_summarised_df()[, 2])
        }

        col_chart_df <- data.frame(bar_x_var, bar_y_var)
        chart <-
          ggplot2::ggplot(
            col_chart_df,
            ggplot2::aes(col_chart_df[, 1], col_chart_df[, 2])
          ) +
          ggplot2::geom_col(color = "#2c3e50", fill = "#2c3e50") +
          ggplot2::xlab(x_lab) +
          ggplot2::ylab(y_lab) +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = NA, colour = NA),
            panel.background = ggplot2::element_rect(fill = NA, colour = "#2c3e50"),
            axis.text.x = ggplot2::element_text(
              angle = -45,
              vjust = 1,
              hjust = 0,
              size = axis_font_size
            ),
            axis.text.y = ggplot2::element_text(size = axis_font_size),
            axis.title.x = ggplot2::element_text(size = lab_font_size),
            axis.title.y = ggplot2::element_text(size = lab_font_size)
          )
      }

      chart
    },
    height = function() {
      input$chart_height
    },
    bg = "transparent"
  )

  # Tonga Map ---------------------------------------------------------------

  data_file_tonga <- reactiveValues(
    tmap_drawn = 0
  )
  # select one table as active layer
  observe({
    df <- input$tonga_data

    tonga_data_tmp <- try(sf::st_layers(input$tonga_data$datapath)$name)

    if (class(tonga_data_tmp) != "try-error") {
      if ("tonga-crops" %in% tonga_data_tmp) {
        tonga_crops <- sf::st_read(input$tonga_data$datapath, layer = "tonga-crops")
        sf::st_crs(tonga_crops) <- 4326

        data_file_tonga$tonga_crop_survey_raw <- tonga_crops
        data_file_tonga$tonga_crop_survey_block <- crop_survey_zonal_stats(tonga_block, tonga_crops)
        data_file_tonga$tonga_crop_survey_village <- crop_survey_zonal_stats(tonga_village, tonga_crops)
        data_file_tonga$tonga_crop_survey_district <- crop_survey_zonal_stats(tonga_district, tonga_crops)
      }

      if ("vavau-crops" %in% tonga_data_tmp) {
        vavau_crops <- sf::st_read(input$tonga_data$datapath, layer = "vavau-crops")
        sf::st_crs(vavau_crops) <- 4326

        data_file_tonga$vavau_crop_survey_raw <- vavau_crops
        data_file_tonga$vavau_crop_survey_block <- crop_survey_zonal_stats(vavau_block, vavau_crops)
        data_file_tonga$vavau_crop_survey_village <- crop_survey_zonal_stats(vavau_village, vavau_crops)
        data_file_tonga$vavau_crop_survey_district <- crop_survey_zonal_stats(vavau_district, vavau_crops)
      }
    }
  })

  # active df
  tonga_active_df <- reactive({
    req(input$tonga_layers)

    if (input$tonga_layers == "Tonga crop survey (raw)") {
      tdf <- data_file_tonga$tonga_crop_survey_raw
    }

    if (input$tonga_layers == "Tonga crop survey (block)") {
      tdf <- data_file_tonga$tonga_crop_survey_block
    }

    if (input$tonga_layers == "Tonga crop survey (village)") {
      tdf <- data_file_tonga$tonga_crop_survey_village
    }

    if (input$tonga_layers == "Tonga crop survey (district)") {
      tdf <- data_file_tonga$tonga_crop_survey_district
    }

    # add layer id for leaflet
    if (!is.null(tdf)) {
      if (nrow(tdf) > 0) {
        tdf$layer_id <- as.character(1:nrow(tdf))
        tdf <- tdf %>%
          dplyr::mutate_if(is.numeric, round, 3)
      }
    }

    tdf
  })

  # Tonga Map --------------------------------------------------------
  # select one table as active layer from files loaded to the server


  observe({
    req(input$tonga_data)

    df <- tonga_active_df()
    choices <- colnames(df)
    updateSelectInput(session,
      "tonga_map_layer",
      choices = choices
    )
  })

  tonga_label_vars <-
    mod_multiple_input_server(
      id = "tonga_label_vars",
      m_df = tonga_active_df
    )

  tonga_map_waiter <- waiter::Waiter$new(
    html = map_screen,
    color = "rgba(44,62,80,.6)"
  )

  output$tonga_leafmap <- leaflet::renderLeaflet({
    base_map <- leaflet::leaflet() %>%
      leaflet::addTiles(group = "OSM (default)") %>%
      leaflet::addProviderTiles(
        providers$Esri.WorldImagery,
        options = providerTileOptions(maxZoom = 17),
        group = "ESRI Satellite"
      ) %>%
      leaflet::setView(lng = -173.985803, lat = -18.627126, zoom = 12) %>%
      leaflet::addLayersControl(
        baseGroups = c("OSM (default)", "ESRI Satellite"),
        options = leaflet::layersControlOptions(collapsed = FALSE),
        position = c("bottomright")
      )
  })

  # add spatial data to Tonga map
  observe({
    req(tonga_active_df())

    # map takes reactive dependency on user specified layer and colour
    tonga_map_layer <- isolate(input$tonga_map_layer)
    map_colour <- isolate(input$tonga_map_colour)

    if ("sf" %in% class(tonga_active_df()) &
      is.atomic(tonga_active_df()[[tonga_map_layer]]) &
      nrow(tonga_active_df()) > 0 &
      input$tonga_data_view == "t_map") {

      # don't draw legend if plot id or zone is selected
      if (tonga_map_layer == "zone" |
        tonga_map_layer == "plot_id") {
        data_file_tonga$tmap_drawn <- 1
        add_layers_leaflet(
          map_object = "tonga_leafmap",
          map_active_df = tonga_active_df(),
          map_var = tonga_map_layer,
          map_colour = map_colour,
          opacity = 0.8,
          map_line_width = 0.25,
          map_line_colour = "blue",
          waiter = tonga_map_waiter
        )
      } else {
        data_file_tonga$tmap_drawn <- 1
        add_layers_leaflet_legend(
          map_object = "tonga_leafmap",
          map_active_df = tonga_active_df(),
          map_var = tonga_map_layer,
          map_colour = map_colour,
          opacity = 0.8,
          map_line_width = 0.25,
          map_line_colour = "blue",
          waiter = tonga_map_waiter
        )
      }
    }
  })

  # update colour
  observeEvent(input$tonga_map_colour, {
    req(tonga_active_df())

    if (data_file_tonga$tmap_drawn == 1) {
      # map takes reactive dependency on user specified layer and colour
      tonga_map_layer <- input$tonga_map_layer
      map_colour <- input$tonga_map_colour

      if ("sf" %in% class(tonga_active_df()) &
        is.atomic(tonga_active_df()[[tonga_map_layer]]) &
        nrow(tonga_active_df()) > 0 &
        input$tonga_data_view == "t_map") {

        # don't draw legend if plot id or zone is selected
        if (tonga_map_layer == "zone" |
          tonga_map_layer == "plot_id") {
          data_file$tmap_drawn <- 1
          add_layers_leaflet_no_zoom(
            map_object = "tonga_leafmap",
            map_active_df = tonga_active_df(),
            map_var = tonga_map_layer,
            map_colour = map_colour,
            opacity = 0.8,
            map_line_width = 0.25,
            map_line_colour = "blue",
            waiter = tonga_map_waiter
          )
        } else {
          data_file_tonga$tmap_drawn <- 1
          add_layers_leaflet_legend_no_zoom(
            map_object = "tonga_leafmap",
            map_active_df = tonga_active_df(),
            map_var = tonga_map_layer,
            map_colour = map_colour,
            opacity = 0.8,
            map_line_width = 0.25,
            map_line_colour = "blue",
            waiter = tonga_map_waiter
          )
        }
      }
    }
  })

  # update layer
  observeEvent(input$tonga_map_layer, {
    req(tonga_active_df())

    if (data_file_tonga$tmap_drawn == 1) {
      # map takes reactive dependency on user specified layer and colour
      tonga_map_layer <- input$tonga_map_layer
      map_colour <- input$tonga_map_colour

      if ("sf" %in% class(tonga_active_df()) &
        is.atomic(tonga_active_df()[[tonga_map_layer]]) &
        nrow(tonga_active_df()) > 0 &
        input$tonga_data_view == "t_map") {

        # don't draw legend if plot id or zone is selected
        if (tonga_map_layer == "zone" |
          tonga_map_layer == "plot_id") {
          data_file_tonga$tmap_drawn <- 1
          add_layers_leaflet_no_zoom(
            map_object = "tonga_leafmap",
            map_active_df = tonga_active_df(),
            map_var = tonga_map_layer,
            map_colour = map_colour,
            opacity = 0.8,
            map_line_width = 0.25,
            map_line_colour = "blue",
            waiter = tonga_map_waiter
          )
        } else {
          data_file_tonga$tmap_drawn <- 1
          add_layers_leaflet_legend_no_zoom(
            map_object = "tonga_leafmap",
            map_active_df = tonga_active_df(),
            map_var = tonga_map_layer,
            map_colour = map_colour,
            opacity = 0.8,
            map_line_width = 0.25,
            map_line_colour = "blue",
            waiter = tonga_map_waiter
          )
        }
      }
    }
  })

  # add popup labels
  observe({
    leaflet::leafletProxy("tonga_leafmap") %>%
      clearPopups()

    # capture click events
    # event_shape captures a user click on a shape object
    # event_marker captures a user click on a marker object
    event_shape <- input$tonga_leafmap_shape_click
    event_marker <- input$tonga_leafmap_marker_click

    # if a user has not clicked on a marker or object leave event as null if a
    # user has clicked on a shape or marker update event and pass it into
    # fct_add_popups to create popup for clicked object
    event <- NULL

    if (!is.null(event_shape)) {
      event <- event_shape
    }

    if (!is.null(event_marker)) {
      event <- event_marker
    }

    if (is.null(event)) {
      return()
    }

    isolate({
      req(tonga_label_vars())

      content <-
        add_popups(
          in_df = tonga_active_df,
          layer_id = event$id,
          label_vars = tonga_label_vars
        )
      print(content)

      leaflet::leafletProxy("tonga_leafmap") %>%
        leaflet::addPopups(
          event$lng,
          event$lat,
          content,
          layerId = event$id
        )
    })
  })

  # Tonga data table --------------------------------------------------------
  tonga_table_vars <-
    mod_multiple_input_server(
      id = "tonga_table_layers",
      m_df = tonga_active_df
    )

  tonga_table_df <- reactive({
    if (is.null(tonga_table_vars())) {
      table_df <- tonga_active_df()
    } else {
      table_df <- tonga_active_df() %>%
        dplyr::select(tonga_table_vars())
    }

    colnames <- colnames(table_df)
    # don't give user the option to select zones or geometry objects for plotting

    if ("geometry" %in% colnames) {
      table_df <- table_df %>%
        sf::st_drop_geometry()
    }

    if ("geom" %in% colnames) {
      table_df <- table_df %>%
        sf::st_drop_geometry()
    }

    if ("layer_id" %in% colnames) {
      table_df <- table_df %>%
        select(-c("layer_id"))
    }

    table_df
  })

  mod_render_dt_server(
    id = "tonga_data_dt",
    dt = tonga_table_df,
    editable = FALSE
  )

  # download tonga table data
  output$download_tonga_data <- downloadHandler(
    filename = function() {
      fname <- input$tonga_layers
      fname <- stringr::str_replace_all(fname, " ", "_")
      paste0(fname, "_", dt(), ".csv")
    },
    content = function(file) {
      req(tonga_active_df())

      readr::write_csv(tonga_active_df(), file)
    }
  )

  # Tonga charts ------------------------------------------------------------
  tonga_chart_df <- reactive({
    req(input$tonga_layers != "Tonga crop survey (raw)")

    tonga_layers <- input$tonga_layers

    # return null dataframe if user has selected raw data
    if (tonga_layers != "Tonga crop survey (raw)") {
      t_chart_df <- tonga_active_df()
    } else {
      t_chart_df <- NULL
    }

    t_chart_df
  })

  observe({
    req(input$tonga_data)

    df <- tonga_chart_df()
    choices <- colnames(df)
    # don't give user the option to select zones or geometry objects for plotting
    choices <- choices[!(choices %in% "zone")]
    choices <- choices[!(choices %in% "plot_id")]
    choices <- choices[!(choices %in% "goem")]
    choices <- choices[!(choices %in% "geometry")]
    choices <- choices[!(choices %in% "layer_id")]

    updateSelectInput(
      session,
      "tonga_chart_layer",
      choices = choices
    )
  })

  output$tonga_chart <- renderPlot(
    {
      req(tonga_chart_df(), input$tonga_chart_layer)

      # take a reactive dependency on user specified chart styling parameters
      lab_font_size <- input$tonga_lab_font
      axis_font_size <- input$tonga_axis_font
      x_lab <- input$tonga_x_axis_label
      y_lab <- input$tonga_y_axis_label
      tonga_chart_df <- data.frame(tonga_chart_df())

      tonga_chart <- ggplot2::ggplot(
        tonga_chart_df,
        ggplot2::aes(.data[["zone"]], .data[[input$tonga_chart_layer]])
      ) +
        ggplot2::geom_col(color = "#2c3e50", fill = "#2c3e50") +
        ggplot2::xlab(x_lab) +
        ggplot2::ylab(y_lab) +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = NA, colour = NA),
          panel.background = ggplot2::element_rect(fill = NA, colour = "#2c3e50"),
          axis.text.x = ggplot2::element_text(
            angle = -45,
            vjust = 1,
            hjust = 0,
            size = axis_font_size
          ),
          axis.text.y = ggplot2::element_text(size = axis_font_size),
          axis.title.x = ggplot2::element_text(size = lab_font_size),
          axis.title.y = ggplot2::element_text(size = lab_font_size)
        )

      tonga_chart
    },
    height = function() {
      input$tonga_chart_height
    },
    bg = "transparent"
  )

  # Admin - data cleaning ---------------------------------------------------

  # user uploaded files for data cleaning / editing
  # return table of files and file paths of data loaded to the server
  upload_edit_file <- mod_get_layers_server(id = "edit_data")

  # update reactiveValues object holding dataframe of layers a user can select as active layer
  observe({
    req(upload_edit_file())

    upload_edit_file <- isolate(upload_edit_file())
    isolate({
      # clear previously uploaded data
      data_file$edit_data_file <- data.frame()
      df <- dplyr::bind_rows(data_file$edit_data_file, upload_edit_file)
      # unique number id next to each layer to catch uploads of tables with same name
      rows <- nrow(df)
      row_idx <- 1:rows
      df$layer_disp_name_idx <-
        paste0(df$layer_disp_name, "_", row_idx, sep = "")
      data_file$edit_data_file <- df

      # create new log for edits
      # log file for any errors
      data_file$edit_log <- NULL
      log <-
        fs::file_temp(
          pattern = "log",
          tmp_dir = tempdir(),
          ext = "txt"
        )
      data_file$edit_log <- log
    })

    # reset map zoom to new data's extent
    data_file$map_edits_zoom <- 0
  })

  # select one table as editing layer from GeoPackage loaded for editing
  observe({
    df <- data_file$edit_data_file
    choices <- unique(df$layer_disp_name_idx)
    updateSelectInput(session, "edit_layer", choices = choices)
  })

  # editing df - use this df editing records
  edit_df <- reactive({
    req(input$edit_layer)
    req(data_file$flush_deletes)
    req(data_file$flush_edits)
    req(data_file$flush_geometry_edits)

    df <- isolate(data_file$edit_data_file)
    edit_df <- read_tables(df, input$edit_layer)

    # check if layer to be edited is spatial
    # if not spatial drop geometry and convert to dataframe
    # if spatial transform to 4326 for leaflet
    if ("sf" %in% class(edit_df)) {
      edit_layer_crs <- sf::st_crs(edit_df)
    }

    if (is.na(edit_layer_crs) & "sf" %in% class(edit_df)) {
      edit_df <- edit_df %>%
        sf::st_drop_geometry() %>%
        as.data.frame()
    } else if (!is.na(edit_layer_crs) & "sf" %in% class(edit_df)) {
      edit_df <- edit_df %>%
        sf::st_transform(4326)
    }

    edit_df
  })

  # render editable layer as a data table
  mod_render_dt_server(id = "edit_data_dt", dt = edit_df, editable = TRUE)

  # add editing layer to map
  output$edit_leafmap <- leaflet::renderLeaflet({
    base_map <- leaflet::leaflet() %>%
      leaflet::addTiles(group = "OSM (default)") %>%
      leaflet::addProviderTiles(
        providers$Esri.WorldImagery,
        options = providerTileOptions(maxZoom = 17),
        group = "ESRI Satellite"
      ) %>%
      leaflet::setView(0, 0, 3) %>%
      leaflet::addLayersControl(
        baseGroups = c("OSM (default)", "ESRI Satellite"),
        options = leaflet::layersControlOptions(collapsed = FALSE),
        position = c("bottomright")
      ) %>%
      leaflet::addMeasure(
        position = "bottomright",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479"
      )

    base_map
  })

  # add spatial data to edit map
  observe({
    req(edit_df())

    # add layerID to edit_df - this to identify features that a user clicks
    edit_df <- edit_df()
    if (nrow(edit_df) > 0) {
      edit_df$layer_id <- as.character(1:nrow(edit_df))
    }

    edit_layer <- isolate(input$edit_layer)

    # only render map if it is spatial (of class sf)
    if ("sf" %in% class(edit_df) &
      nrow(edit_df) > 0 &
      input$edit_data_view == "edit_map" &
      data_file$map_edits_zoom == 0) {
      edit_map_proxy <- add_layers_leaflet(
        map_object = "edit_leafmap",
        map_active_df = edit_df,
        map_var = edit_layer,
        map_colour = "#00ffff",
        opacity = 0.75,
        map_line_width = 0.15,
        map_line_colour = "blue",
        waiter = map_waiter
      )
    } else if ("sf" %in% class(edit_df) &
      nrow(edit_df) > 0 &
      input$edit_data_view == "edit_map" &
      data_file$map_edits_zoom > 0) {
      edit_map_proxy <- add_layers_leaflet_no_zoom(
        map_object = "edit_leafmap",
        map_active_df = edit_df,
        map_var = edit_layer,
        map_colour = "#00ffff",
        opacity = 0.75,
        map_line_width = 0.15,
        map_line_colour = "blue",
        waiter = map_waiter
      )
    }

    # show warning if edit is not spatial (class sf)
    shinyFeedback::feedbackWarning(
      "edit_layer",
      !("sf" %in% class(edit_df)),
      "Not a spatial layer"
    )
  })

  # listen for user clicking a feature to edit
  observe({
    # capture click events
    # event_shape captures a user click on a shape object
    # event_marker captures a user click on a marker object
    event_shape <- input$edit_leafmap_shape_click
    event_marker <- input$edit_leafmap_marker_click

    # if a user has not clicked on a marker or object leave event as null if a
    # user has clicked on a shape or marker update event and pass it into
    event <- NULL

    if (!is.null(event_shape)) {
      event <- event_shape
    }

    if (!is.null(event_marker)) {
      event <- event_marker
    }

    if (is.null(event)) {
      return()
    }

    event <- event$id
    data_file$event_tmp <- as.numeric(event)
    edit_df <- isolate(edit_df())
    feature <- edit_df[as.numeric(event), ]
    feature$layer_id <- 1

    # get bounding box for the map
    bbox <- sf::st_bbox(feature) %>%
      as.vector()

    # add editing feature to map
    output$edit_zoommap <- leaflet::renderLeaflet({
      zoom_map <- leaflet::leaflet() %>%
        leaflet::addTiles(group = "OSM (default)") %>%
        leaflet::addProviderTiles(
          providers$Esri.WorldImagery,
          options = providerTileOptions(maxZoom = 17),
          group = "ESRI Satellite"
        ) %>%
        leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) %>%
        leaflet::addLayersControl(
          baseGroups = c("OSM (default)", "ESRI Satellite"),
          options = leaflet::layersControlOptions(collapsed = FALSE),
          position = c("bottomright")
        ) %>%
        leaflet::addPolygons(
          data = feature,
          group = "feature",
          layerId = feature$layer_id
        ) %>%
        leaflet.extras::addDrawToolbar(
          targetGroup = "feature",
          polylineOptions = FALSE,
          polygonOptions = FALSE,
          circleOptions = FALSE,
          rectangleOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          editOptions = leaflet.extras::editToolbarOptions()
        )
      zoom_map
    })

    showModal(
      modalDialog(
        tags$h4("Edit feature"),
        leafletOutput("edit_zoommap"),
        modalButton("close"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  # Edited Features
  observeEvent(input$edit_zoommap_draw_edited_features, {
    req(edit_df())
    edits <- input$edit_zoommap_draw_edited_features
    # convert return from leaflet.draw to json so it can be read by st_read
    edits_to_json <- jsonlite::toJSON(edits, auto_unbox = TRUE, force = TRUE, digits = NA)
    edits_to_sf <- sf::st_read(edits_to_json)

    edit_df <- edit_df()
    colnames <- colnames(edit_df)
    row_to_edit <- data_file$event_tmp

    if ("sf" %in% class(edit_df)) {
      if ("geom" %in% colnames) {
        geom <- edits_to_sf$geometry
        edit_df[row_to_edit, ]$geom <- geom
      }

      if ("geometry" %in% colnames) {
        geom <- edits_to_sf$geometry
        edit_df[row_to_edit, ]$geometry <- geom
      }
    }

    write_tables(edit_df, isolate(data_file$edit_data_file), input$edit_layer)
    data_file$map_edits_zoom <- data_file$map_edits_zoom + 1
    data_file$flush_geometry_edits <- data_file$flush_geometry_edits + 1
    data_file$event_tmp <- NULL
  })

  observeEvent(input$edit_zoommap_draw_deleted_features, {
    req(edit_df())
    deletes <- input$edit_zoommap_draw_deleted_features
    deletes_to_json <- jsonlite::toJSON(deletes, auto_unbox = TRUE, force = TRUE, digits = NA)
    deletes_to_sf <- sf::st_read(deletes_to_json)

    edit_df <- edit_df()
    row_to_delete <- data_file$event_tmp

    if (nrow(deletes_to_sf) > 0) {
      edit_df <- edit_df %>%
        dplyr::filter(dplyr::row_number() != row_to_delete)
      write_tables(edit_df, isolate(data_file$edit_data_file), input$edit_layer)
      data_file$flush_geometry_edits <- data_file$flush_geometry_edits + 1
    }

    data_file$map_edits_zoom <- data_file$map_edits_zoom + 1
    data_file$event_tmp <- NULL
  })


  delete_waiter <- waiter::Waiter$new(
    html = delete_screen,
    color = "rgba(44,62,80,.6)"
  )

  # delete selected rows from GeoPackage
  observeEvent(input$delete_records, {
    
    delete_waiter$show()
    selected_rows <- input$`edit_data_dt-data_table_rows_selected`
    id_str <- input$row_id
    edit_df_colnames <- colnames(edit_df())

    # layer that is edited prior to any deletions - keep to preserve row indexes
    pre_edit_df <- edit_df()

    # check if there is a unique column id that can be used to propagate deletes through related tables
    if (nchar(id_str) > 0) {
      # find column in edit_df that stores row ids
      id_col <- stringr::str_detect(edit_df_colnames, id_str)
      id_col <- edit_df_colnames[id_col]
      if (length(id_col) > 1) {
        id_col <- NULL
      }
    } else {
      id_col <- NULL
    }

    shinyFeedback::feedbackWarning("row_id", is.null(id_col), "Cannot uniquely identify id column")

    if (length(selected_rows) > 0 & !is.null(id_col)) {
      for (i in selected_rows) {
        # this is unique row id that defines row to be deleted
        id_to_delete <- pre_edit_df[i, ][[id_col]]

        # only delete rows if selected row-column is not null or NA
        if (!is.na(id_to_delete) & !is.null(id_to_delete)) {
          # get layers
          edit_gpkg <- data_file$edit_data_file
          layers <- unique(edit_gpkg$layer_disp_name_idx)
          # iterate over layers, delete row, and sync changes to GeoPackage
          for (ii in layers) {
            tmp_edit_df <- read_tables(isolate(data_file$edit_data_file), ii)
            tmp_edit_df_colnames <- colnames(tmp_edit_df)
            tmp_edit_df_id_col <- stringr::str_detect(tmp_edit_df_colnames, id_str)
            tmp_edit_df_id_col <- tmp_edit_df_colnames[tmp_edit_df_id_col]

            if (length(tmp_edit_df_id_col) > 1) {
              tmp_edit_df_id_col <- NULL
              # write delete fail error message to log
              readr::write_lines(
                paste0("cannot delete row in layer: ", ii, " - cannot uniquely identify id column"),
                data_file$edit_log,
                sep = "\n",
                na = "NA",
                append = TRUE
              )
            } else if (length(tmp_edit_df_id_col) == 0) {
              tmp_edit_df_id_col <- NULL
              # write delete fail error message to log
              readr::write_lines(
                paste0("cannot delete row in layer: ", ii, " - id column missing"),
                data_file$edit_log,
                sep = "\n",
                na = "NA",
                append = TRUE
              )
            } else {
              delete_message <- tryCatch(
                error = function(cnd) paste0("cannot delete row-id: ", id_to_delete, "from layer: ", ii),
                {
                  # include | condition in filter as dplyr filter removes NA rows - this prevents that from occurring
                  # this should not present unexpected behaviour as users are prevented from deleting NA rows
                  tmp_edit_df <- tmp_edit_df %>%
                    dplyr::filter(.data[[tmp_edit_df_id_col]] != id_to_delete | is.na(.data[[tmp_edit_df_id_col]]))
                  write_tables(tmp_edit_df, isolate(data_file$edit_data_file), ii)
                  paste0("deleted row-id: ", id_to_delete, "from layer: ", ii)
                }
              )
              readr::write_lines(
                delete_message,
                data_file$edit_log,
                sep = "\n",
                na = "NA",
                append = TRUE
              )
            }
          }
        }
      }
    }
    data_file$flush_deletes <- data_file$flush_deletes + 1
    delete_waiter$hide()
  })

  # sync forms with database / template
  edit_waiter <- waiter::Waiter$new(
    html = edit_screen,
    color = "rgba(44,62,80,.6)"
  )

  # record edits before updating GeoPackage
  observeEvent(input$`edit_data_dt-data_table_cell_edit`, {
    tmp_edits <- data_file$tmp_edits
    edited_rows <- input$`edit_data_dt-data_table_cell_edit`
    tmp_edits <- dplyr::bind_rows(tmp_edits, edited_rows)
    data_file$tmp_edits <- tmp_edits
  })

  # apply edits and updating GeoPackage
  observeEvent(input$save_edits, {

    # layer that is edited prior to any edits
    df_to_edit <- edit_df()

    if ("sf" %in% class(df_to_edit)) {
      df_to_edit_not_sf <- df_to_edit %>%
        sf::st_drop_geometry()
    } else {
      df_to_edit_not_sf <- df_to_edit
    }

    # edits
    tmp_edits <- data_file$tmp_edits

    if (nrow(tmp_edits) > 0) {
      edit_waiter$show()
      edits <- edit_data_frame(
        tmp_edits,
        df_to_edit,
        df_to_edit_not_sf,
        input$edit_layer
      )
      df_to_edit <- edits[[1]]
      write_tables(df_to_edit, isolate(data_file$edit_data_file), input$edit_layer)
      edits_log <- edits[[2]]
      for (i in edits_log) {
        readr::write_lines(
          i,
          data_file$edit_log,
          sep = "\n",
          na = "NA",
          append = TRUE
        )
      }
      edit_waiter$hide()
    }

    # reset edits object to empty after applying them GeoPackage
    data_file$tmp_edits <- data.frame()
    data_file$flush_edits <- data_file$flush_edits + 1
  })

  # apply edits and update GeoPackage upon change in editing layer
  observeEvent(input$edit_layer, {
    # layer that is edited prior to any edits
    df_to_edit <- edit_df()

    if ("sf" %in% class(df_to_edit)) {
      df_to_edit_not_sf <- df_to_edit %>%
        sf::st_drop_geometry()
    } else {
      df_to_edit_not_sf <- df_to_edit
    }

    # edits
    tmp_edits <- data_file$tmp_edits

    if (nrow(tmp_edits) > 0) {
      edit_waiter$show()
      edits <- edit_data_frame(
        tmp_edits,
        df_to_edit,
        df_to_edit_not_sf,
        input$edit_layer
      )
      df_to_edit <- edits[[1]]
      write_tables(df_to_edit, isolate(data_file$edit_data_file), input$edit_layer)
      edits_log <- edits[[2]]
      for (i in edits_log) {
        readr::write_lines(
          i,
          data_file$edit_log,
          sep = "\n",
          na = "NA",
          append = TRUE
        )
      }
      edit_waiter$hide()
    }

    # reset edits object to empty after applying them GeoPackage
    data_file$tmp_edits <- data.frame()
    data_file$flush_edits <- data_file$flush_edits + 1
  })

  # download edited data as a zip file
  output$download_edits <- downloadHandler(
    filename = function() {
      req(nrow(data_file$edit_data_file) >= 1)

      paste("edited_", dt(), ".zip", sep = "")
    },
    content = function(file) {
      req(nrow(data_file$edit_data_file) >= 1)

      dpath <- data_file$edit_data_file
      # row 1 should be the path the geopackage that is being edited as a user can only load and edit one geopackage at a time
      dpath <- dpath[1, ][["file_path"]]
      log_path <- data_file$edit_log
      zip(
        zipfile = file,
        files = c(dpath, log_path),
        flags = "-r9Xj"
      )
    },
    contentType = "application/zip"
  )
}
