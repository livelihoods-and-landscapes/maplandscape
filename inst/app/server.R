

options(shiny.maxRequestSize = 5000 * 1024^2)

app_server <- function(input, output, session) {

  # hold the loading screen for 1 second
  Sys.sleep(1)

  # hide loading screen
  shinyjs::hide(
    id = "loading-screen",
    anim = TRUE,
    animType = "fade"
    )

  # move to data tab when action button pressed on landing page
  observeEvent(input$enter, {
    updateNavbarPage(session, "navbar", selected = "Data")
  })


  # app data ----------------------------------------------------------------

  # object storing data to pass between reactive objects during app's execution
  app_data <-
    reactiveValues(
      data_file = data.frame(), # dataframe of listing layers and temporary locations of data a user has uploaded
      map_drawn = 0, # indicator for state of map (if 0, zoom to data's bbox)
      joined_df = list(), # list of layers created through joins, spatial joins, or filtering rows
      buckets = NULL, # list of buckets in users GCS project
      items = NULL, # items in GCS bucket
      flush_add_column = 0, # trigger re-render of active layer in data table after adding column
      edit_data_file = data.frame(), # data frame / spatial layer to edit
      tmp_edits = data.frame(), # temporary record of user edits to records in Data Table
      edit_log = NULL,
      flush_deletes = 1, # trigger reload of data and map after deleting features
      flush_edits = 1, # trigger reload of data and map after editing features
      flush_geometry_edits = 1, # trigger reload of data and map after editing geometries
      event_tmp = NULL, # events caused by user clicking on edit_leafmap to edit features
      map_edits_zoom = 0,
      admin_buckets = NULL, # name of GCS buckets in GCS project
      admin_fname = NULL, # name of GeoPackage currently being edited
      admin_current_bucket = NULL # name of GCS bucket where GeoPackage currently being edited is stored
    )

  # Data Sync ---------------------------------------------------------------

  # sync forms with database / template
  sync_waiter <- waiter::Waiter$new(
    html = sync_screen,
    color = "rgba(44,62,80,.6)"
  )

  # select template db to sync forms to
  template <- mod_get_layers_Server(id = "template_db")

  # forms to sync to template db
  forms <- mod_get_layers_Server(id = "forms_db")

  # returns 4 element list
  # element 1 is file name and path to temporary geopackage
  # element 2 is date-time string for creation of temporary geopackage
  # element 3 is a data frame in the same format as returned by shiny::fileUpload
  # element 4 is a log file of the syncing process
  sync_gpkg_path <- reactive({
    req(
      template(),
      forms()
    )

    sync_waiter$show()
    sync_gpkg_path <- sync_forms(
      template = template(),
      forms = forms()
    )
    sync_waiter$hide()

    sync_gpkg_path
  })

  # download raw synced data as a zip file
  output$download_sync_forms <- downloadHandler(
    filename = function() {
      req(sync_gpkg_path()[[1]])

      paste("synced_forms_", sync_gpkg_path()[[2]], ".zip", sep = "")
    },
    content = function(file) {
      req(sync_gpkg_path()[[1]])

      zip(
        zipfile = file,
        files = c(sync_gpkg_path()[[1]], sync_gpkg_path()[[4]]),
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
        mod_get_layers_UI(
          id = "template_db",
          label = "Select template .gpkg",
          multiple = FALSE,
          accept = c(".gpkg")
        ),
        tags$h4("Completed forms"),
        mod_get_layers_UI(
          id = "forms_db",
          label = "Select forms .gpkg",
          multiple = TRUE,
          accept = c(".gpkg")
        ),
        hr(),
        checkboxInput(
          "add_synced_forms",
          label = "add synced forms to active layer",
          value = TRUE
        ),
        downloadButton(
          "download_sync_forms",
          "Download"
        ),
        hr(),
        modalButton("Go to app"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  # Data Upload -------------------------------------------------------------

  # add synced files to app
  sync_file <- reactive({
    req(
      sync_gpkg_path()[[1]],
      input$add_synced_forms
    )

    sync_file <- sync_gpkg_path()[[3]]
    sync_file
  })

  # update app_data data_file object of layers a user can select as active layer with synced data
  observe({
    req(sync_file())

    sync_file <- isolate(sync_file())
    isolate({
      df <- dplyr::bind_rows(
        app_data$data_file,
        sync_file
      )
      # unique number id next to each layer to distinguish uploads of layers with same name
      rows <- nrow(df)
      row_idx <- 1:rows
      df$layer_disp_name_idx <-
        paste0(df$layer_disp_name, "_", row_idx, sep = "")
      app_data$data_file <- df
    })
  })

  # user uploaded files
  # return table of files and file paths of data loaded to the server
  upload_file <- mod_get_layers_Server(id = "user_data")

  # update app_data data_file object of layers a user can select as active layer with user uploaded data
  observe({
    req(upload_file())

    upload_file <- isolate(upload_file())
    isolate({
      df <- dplyr::bind_rows(
        app_data$data_file,
        upload_file
      )
      # unique number id next to each layer to catch uploads of tables with same name
      rows <- nrow(df)
      row_idx <- 1:rows
      df$layer_disp_name_idx <-
        paste0(df$layer_disp_name, "_", row_idx, sep = "")
      app_data$data_file <- df
    })
  })

  # Get GeoPackages from Google Cloud Storage

  # display login with Google button if token is not valid
  output$login_warning <- renderUI({
    if (is.null(isolate(token()))) {
      tags$p("WARNING: login with Google will reset the app and current data will be lost")
    } else {
      return()
    }
  })

  output$login_button <- renderUI({
    if (is.null(isolate(token()))) {
      tags$a("Login with Google",
        href = url,
        class = "btn btn-outline-danger m-2"
      )
    } else {
      return()
    }
  })

  ## Get token that can be used to make authenticated requests to Google Cloud Storage
  token <- reactive({

    ## gets all the parameters in the URL. The auth code should be one of them.
    pars <- shiny::parseQueryString(session$clientData$url_search)

    if (length(pars$code) > 0) {
      ## extract the authorization code
      # Manually create a token
      token <- try(
        httr::oauth2.0_token(
          app = app,
          endpoint = api,
          credentials = httr::oauth2.0_access_token(api, app, pars$code),
          cache = TRUE
        )
      )
      if ("try-error" %in% class(token)) {
        token <- NULL
      }
    } else {
      token <- NULL
    }

    token
  })

  # get list of GCS Buckets
  observeEvent(input$list_google_files, {
    req(token())
    req(input$gcs_project_id)

    buckets <- list_gcs_buckets(
      token(),
      input$gcs_project_id
    )

    if ("no items returned" %in% buckets | is.null(buckets)) {
      shiny::showNotification(
        "no buckets available in Google Cloud Storage",
        type = "error",
        duration = 5
      )
    } else {
      app_data$buckets <- buckets
    }
  })

  # update select input with list of objects in Google Cloud Storage bucket
  observe({
    app_data$buckets

    if (length(app_data$buckets) > 0 & !"no items returned" %in% app_data$buckets) {
      updateSelectInput(
        session,
        "gcs_bucket_name",
        choices = app_data$buckets
      )
    }
  })

  observe({
    req(input$gcs_bucket_name)

    items <- list_gcs_bucket_objects(
      token(),
      input$gcs_bucket_name
    )

    if ("no items returned" %in% items | is.null(items)) {
      shiny::showNotification(
        "no items returned from Google Cloud Storage query",
        type = "error",
        duration = 5
      )
      app_data$items <- NULL
    } else {
      app_data$items <- items
    }
  })

  # update select input with list of objects in Google Cloud Storage bucket
  observe({
    app_data$items

    if (length(app_data$items) > 0 & !"no items returned" %in% app_data$items) {
      updateSelectInput(
        session,
        "gcs_bucket_objects",
        choices = app_data$items
      )
    } else {
      updateSelectInput(
        session,
        "gcs_bucket_objects",
        choices = ""
      )
    }
  })

  # add user selected Google Cloud Storage object to list of layers
  # write GeoPackage retrieved from Google Cloud Storage to app_data$data_file and unpack layers in GeoPackage
  observeEvent(input$get_objects, {
    req(input$gcs_bucket_objects)

    selected_gcs_object <- input$gcs_bucket_objects

    gcs_gpkg <- NULL
    gcs_gpkg <- try(
      get_gcs_object(
        token(),
        input$gcs_bucket_name,
        selected_gcs_object
      )
    )

    f_lyrs <- NULL

    if (!"try-error" %in% class(gcs_gpkg) & !is.null(gcs_gpkg) & !any(stringr::str_detect(gcs_gpkg, "cannot load GeoPackage from Google Cloud Storage"))) {
      f_lyrs <- tryCatch(
        error = function(cnd) NULL,
        purrr::map2(
          gcs_gpkg$f_path,
          gcs_gpkg$f_name,
          list_layers
        ) %>%
          dplyr::bind_rows()
      )

      isolate({
        df <- dplyr::bind_rows(
          app_data$data_file,
          f_lyrs
        )
        # unique number id next to each layer to catch uploads of tables with same name
        rows <- nrow(df)
        row_idx <- 1:rows
        df$layer_disp_name_idx <-
          paste0(df$layer_disp_name, "_", row_idx, sep = "")
        app_data$data_file <- df
      })
    }
  })

  # select one table as active layer from files loaded to the server
  observe({
    df <- app_data$data_file
    joined_df <- app_data$joined_df
    nm_jdf <- names(joined_df)
    choices <- unique(df$layer_disp_name_idx)
    choices <- c(choices, nm_jdf)
    updateSelectInput(
      session,
      "active_layer",
      choices = choices
    )
  })

  # active layer - layer to display in Data Table
  active_df <- reactive({
    req(input$active_layer)

    # update table after add column operation
    update_table <- add_column_count()

    df <- isolate(app_data$data_file)
    jdf <- isolate(names(app_data$joined_df))

    if (any(jdf == input$active_layer)) {
      active_df <- app_data$joined_df[[input$active_layer]]
    } else {
      active_df <- read_tables(df, input$active_layer)
    }

    active_df
  })

  # render active df as raw data table
  mod_render_dt_Server(
    id = "data_raw",
    dt = active_df,
    editable = FALSE
  )

  # Summary Tables ----------------------------------------------------------
  # Perform group-by and summarise operations on active layer

  # Select input for grouping and summarising variables
  grouping_vars <-
    mod_multiple_input_Server(
      id = "grouping_var",
      m_df = active_df
    )

  # filter out selected grouping variables in list of variables which can be summarised
  s_active_df <- reactive({
    req(active_df(), grouping_vars())

    tmp_df <- active_df() %>%
      dplyr::select_if(is.numeric)
    choices <- names(tmp_df)
    s_intersect <- intersect(
      choices,
      grouping_vars()
    )
    choices <- choices[!choices %in% s_intersect]

    choices
  })

  summarising_vars <-
    mod_multiple_input_Server(
      id = "summarising_var",
      m_df = s_active_df
    )

  # perform group-by and summarise operation
  summarised_df <- reactive({
    req(active_df())

    summarised_df <-
      group_by_summarise(
        active_df(),
        grouping_vars(),
        summarising_vars()
      )

    summarised_df
  })

  # render summarised_df as data table
  mod_render_dt_Server(
    id = "data_summary",
    dt = summarised_df,
    editable = FALSE
  )

  # Joining Tables ----------------------------------------------------------
  # combine layers using spatial and non-spatial joins

  # Non-spatial (key-based) joins
  # select "left" table in join operation
  observe({
    df <- app_data$data_file
    joined_df <- app_data$joined_df
    choices <- unique(df$layer_disp_name_idx)
    nm_jdf <- names(joined_df)
    choices <- c(choices, nm_jdf)
    app_data$table_left <- choices

    updateSelectInput(
      session,
      "table_left",
      choices = choices
    )
  })

  left_df <- reactive({
    req(input$table_left)

    df <- isolate(app_data$data_file)
    jdf <- isolate(names(app_data$joined_df))

    if (any(jdf == input$table_left)) {
      left_df <- isolate(app_data$joined_df[[input$table_left]])
    } else {
      left_df <- read_tables(
        df,
        input$table_left
      )
    }

    left_df
  })

  # select "right" table in join
  observe({
    df <- app_data$table_left
    choices <- unique(df)
    choices <- choices[choices != input$table_left]

    updateSelectInput(
      session,
      "table_right",
      choices = choices
    )
  })

  right_df <- reactive({
    req(input$table_right)

    df <- isolate(app_data$data_file)
    jdf <- isolate(names(app_data$joined_df))

    if (any(jdf == input$table_right)) {
      right_df <- isolate(app_data$joined_df[[input$table_right]])
    } else {
      right_df <- read_tables(
        df,
        input$table_right
      )
    }

    right_df
  })

  # update select input for table left primary key
  p_key <-
    mod_multiple_input_Server(
      id = "joining_p_key_left",
      m_df = left_df
    )

  # update select input for table right foreign key
  f_key <-
    mod_multiple_input_Server(
      id = "joining_f_key_right",
      m_df = right_df
    )

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

    app_data$joined_df[[input$join_tbl_name]] <- joined_table
  })

  # Spatial joins
  # select tables for spatial joins
  # select "left" table in spatial join
  observe({
    df <- app_data$data_file
    joined_df <- app_data$joined_df
    choices <- unique(df$layer_disp_name_idx)
    nm_jdf <- names(joined_df)
    choices <- c(choices, nm_jdf)
    app_data$spatial_table_left <- choices

    updateSelectInput(
      session,
      "spatial_table_left",
      choices = choices
    )
  })

  spatial_left_df <- reactive({
    req(input$spatial_table_left)

    df <- isolate(app_data$data_file)
    jdf <- isolate(names(app_data$joined_df))

    if (any(jdf == input$spatial_table_left)) {
      left_df <- isolate(app_data$joined_df[[input$spatial_table_left]])
    } else {
      left_df <- read_tables(
        df,
        input$spatial_table_left
      )
    }

    shinyFeedback::feedbackWarning(
      "spatial_table_left",
      !("sf" %in% class(left_df)),
      "Not a spatial layer"
    )

    left_df
  })

  # select "right" table in spatial join
  observe({
    req(input$spatial_table_left)
    df <- app_data$spatial_table_left

    choices <- unique(df)
    choices <- choices[choices != input$spatial_table_left]

    updateSelectInput(
      session,
      "spatial_table_right",
      choices = choices
    )
  })

  spatial_right_df <- reactive({
    req(input$spatial_table_right)

    df <- isolate(app_data$data_file)
    jdf <- isolate(names(app_data$joined_df))

    if (any(jdf == input$spatial_table_right)) {
      right_df <-
        isolate(app_data$joined_df[[input$spatial_table_right]])
    } else {
      right_df <- read_tables(
        df,
        input$spatial_table_right
      )
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

    app_data$joined_df[[input$spjoin_tbl_name]] <- joined_table
  })

  # Filter Rows based on a condition -------------------------------------------------------------
  # filter modal
  observeEvent(input$filter, {
    showModal(
      modalDialog(
        tags$h4("Filter Options"),
        textInput(
          inputId = "filter_conditions",
          label = "Conditions to filter rows"
        ),
        textInput(
          inputId = "filter_tbl_name",
          "Layer name",
          value = "",
          placeholder = "enter layer name for output"
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
          tags$li("Quotes for strings ~ \"string\""),
          tags$li("Escape apostrophes within strings ~ \"vava\\'u\""),
          tags$li("Specify column names without quotes"),
          tags$li("== ~ equal to"),
          tags$li("!= ~ not equal to"),
          tags$li("<, >, <=, >= ~ greater than / less than comparisons"),
          tags$li("& ~ and"),
          tags$li("| ~ or")
        ),
        tags$p("Example: crop_number > 25"),
        tags$p("Example: island == \"vava\'u\""),
        actionButton(
          "execute_filter",
          "Filter"
        ),
        modalButton("close"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })


  # select tables for row filtering
  observe({
    df <- app_data$data_file
    joined_df <- app_data$joined_df
    choices <- unique(df$layer_disp_name_idx)
    nm_jdf <- names(joined_df)
    choices <- c(choices, nm_jdf)

    updateSelectInput(
      session,
      "table_filter",
      choices = choices
    )
  })


  filter_df <- reactive({
    req(input$table_filter)

    df <- isolate(app_data$data_file)
    jdf <- isolate(names(app_data$joined_df))

    if (any(jdf == input$table_filter)) {
      filter_df <- isolate(app_data$joined_df[[input$table_filter]])
    } else {
      filter_df <- read_tables(
        df,
        input$table_filter
      )
    }

    filter_df
  })

  # execute filter and add filtered table to active layers
  observeEvent(input$execute_filter, {
    req(filter_df())
    req(input$filter_conditions)

    filter_df <- isolate(filter_df())
    filter_out <- filter_rows(filter_df, input$filter_conditions)

    # catch cases when the user does not provide a layer name for the output
    if (nchar(input$filter_tbl_name) < 1) {
      shiny::showNotification(
        "error filtering rows - no output layer name",
        type = "error",
        duration = 5
      )
      removeModal()

      return()
    }

    if (length(input$filter_tbl_name) < 1) {
      shiny::showNotification(
        "error filtering rows - no output layer name",
        type = "error",
        duration = 5
      )
      removeModal()

      return()
    }

    if (is.null(input$filter_tbl_name)) {
      shiny::showNotification(
        "error filtering rows - no output layer name",
        type = "error",
        duration = 5
      )
      removeModal()

      return()
    }

    if (is.character(filter_out) & ("filter error" %in% filter_out)) {
      shiny::showNotification(
        "error filtering rows",
        type = "error",
        duration = 5
      )
      removeModal()

      return()
    }

    # filter_rows() should return an object of class data frame
    # sf objects extend class data frame
    if ("data.frame" %in% class(filter_out)) {
      app_data$joined_df[[input$filter_tbl_name]] <- filter_out
      shiny::showNotification(
        "filter complete - new table in active layers",
        type = "message",
        duration = 5
      )
      removeModal()
    } else {
      shiny::showNotification(
        "error filtering rows - check condition and column names",
        type = "error",
        duration = 5
      )
      removeModal()
    }
  })

  # Add Columns -------------------------------------------------------------

  # Create a new column by combining values from existing columns in a table
  # add column modal
  observeEvent(input$add_column, {
    showModal(
      modalDialog(
        tags$h4("Add New Column"),
        textInput(
          inputId = "col_name",
          label = "New column name"
        ),
        textInput(
          inputId = "mutate_conditions",
          label = "Function to add new column"
        ),
        tags$p(
          "DEMO SNIPPET:"
        ),
        tags$code(
          "area * (crop_percentage / 100)"
        ),
        tags$p("Function to add new column must use dplyr syntax."),
        tags$p("Example: acres * 4046.86"),
        tags$p("Example: tree_number > 0"),
        actionButton(
          "execute_mutate",
          "Create column"
        ),
        modalButton("close"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  # select table to add column to
  observe({
    df <- app_data$data_file
    joined_df <- app_data$joined_df
    choices <- unique(df$layer_disp_name_idx)
    nm_jdf <- names(joined_df)
    choices <- c(choices, nm_jdf)

    updateSelectInput(
      session,
      "table_mutate",
      choices = choices
    )
  })

  mutate_df <- reactive({
    req(input$table_mutate)

    df <- isolate(app_data$data_file)
    jdf <- isolate(names(app_data$joined_df))

    if (any(jdf == input$table_mutate)) {
      mutate_df <- isolate(app_data$joined_df[[input$table_mutate]])
    } else {
      mutate_df <- read_tables(
        df,
        input$table_mutate
      )
    }

    mutate_df
  })

  # execute mutate and add column to selected table
  observeEvent(input$execute_mutate, {
    req(mutate_df())
    req(input$mutate_conditions)

    mutate_df <- isolate(mutate_df())
    df <- isolate(app_data$data_file)
    jdf <- isolate(names(app_data$joined_df))
    col_name <- input$col_name

    mutate_out <- add_column(mutate_df, input$mutate_conditions, col_name)

    # catch cases when the user does not provide a column name
    if (nchar(input$col_name) < 1) {
      shiny::showNotification(
        "error adding column - no column name",
        type = "error",
        duration = 5
      )
      removeModal()

      return()
    }

    if (length(input$col_name) < 1) {
      shiny::showNotification(
        "error adding column - no column name",
        type = "error",
        duration = 5
      )
      removeModal()

      return()
    }

    if (is.null(input$col_name)) {
      shiny::showNotification(
        "error adding column - no column name",
        type = "error",
        duration = 5
      )
      removeModal()

      return()
    }

    if (is.character(mutate_out) & ("mutate error" %in% mutate_out)) {
      shiny::showNotification(
        "error adding column - check condition",
        type = "error",
        duration = 5
      )
      removeModal()

      return()
    }

    # add_column() should return an object of class data frame
    # sf objects extend class data frame
    if ("data.frame" %in% class(mutate_out)) {
      if (any(jdf == input$table_mutate)) {
        # update joined_df object with new column
        app_data$joined_df[[input$table_mutate]] <- mutate_out
        removeModal()
      } else {
        tryCatch(
          error = function(cnd) {
            "mutate error"
          },
          {
            # update data frame / layer if table to add column to is stored in temporary location
            a_lyr <- df %>%
              dplyr::filter(layer_disp_name_idx == input$table_mutate)
            layer <- a_lyr$layers
            sf::st_write(
              mutate_out,
              dsn = a_lyr$file_path,
              layer = layer,
              append = FALSE
            )
            app_data$flush_add_column <- app_data$flush_add_column + 1

          }
        )
        removeModal()
      }
    }
  })

  # counter that is updated after each add column operation
  # used to trigger re-render of data table if it is the active layer
  add_column_count <- reactive({
    req(app_data$flush_add_column)
    count <- app_data$flush_add_column
    count
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

      readr::write_csv(
        active_df(),
        file
      )
    }
  )

  # download summarised data
  output$download_data_summarised <- downloadHandler(
    filename = function() {
      paste("summarised_data_", dt(), ".csv", sep = "")
    },
    content = function(file) {
      req(summarised_df())

      readr::write_csv(
        summarised_df(),
        file
      )
    }
  )

  # Web Map -----------------------------------------------------------------

  # Map options
  # update select input for mapping layer
  observe({
    df <- app_data$data_file
    joined_df <- app_data$joined_df
    nm_jdf <- names(joined_df)
    choices <- unique(df$layer_disp_name_idx)
    choices <- c(choices, nm_jdf)
    updateSelectInput(
      session,
      "map_active_layer",
      choices = choices
    )
  })

  # map_active_df - use this layer for rendering on web map
  map_active_df <- reactive({
    req(input$map_active_layer)

    df <- isolate(app_data$data_file)
    jdf <- isolate(names(app_data$joined_df))

    if (any(jdf == input$map_active_layer)) {
      map_active_df <-
        isolate(app_data$joined_df[[input$map_active_layer]])
    } else {
      map_active_df <- read_tables(
        df,
        input$map_active_layer
      )
    }

    if (nrow(map_active_df) > 0) {
      map_active_df$layer_id <- as.character(1:nrow(map_active_df))
    }

    # To-Do - change to use web GL for rendering large spatial data layers
    if (nrow(map_active_df) > 10000) {
      map_active_df <- map_active_df[1:10000, ]
      shiny::showNotification(
        "Only drawing first 10000 features!",
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
    mod_single_input_Server(
      id = "map_var",
      s_df = map_active_df
    )

  label_vars <-
    mod_multiple_input_Server(
      id = "label_vars",
      m_df = map_active_df
    )

  # Create web map
  output$web_map <- leaflet::renderLeaflet({
    base_map <- leaflet::leaflet() %>%
      leaflet::addTiles(group = "OSM (default)") %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldImagery,
        options = leaflet::providerTileOptions(maxZoom = 17),
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

    # map_drawn is a variable to keep track of the state of the map.
    # 0 = this is the first time data has been drawn on the map for this user session.
    # if map_drawn == 0 on rendering map zoom to data's bbox.
    if (app_data$map_drawn == 0) {
      if ("sf" %in% class(map_active_df()) &
        is.atomic(map_active_df()[[map_var()]]) &
        nrow(map_active_df()) > 0) {
        app_data$map_drawn <- 1
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
        if (any(is.na(sf::st_crs(map_active_df())))) {
          app_data$map_drawn <- 0
        }
      }
    } else if (app_data$map_drawn == 1) {
      if ("sf" %in% class(map_active_df()) &
        is.atomic(map_active_df()[[map_var()]]) &
        nrow(map_active_df()) > 0) {
        app_data$map_drawn <- 1
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
        if (any(is.na(sf::st_crs(map_active_df())))) {
          app_data$map_drawn <- 0
        }
      }
    }

    updateCheckboxInput(
      session,
      "legend",
      value = FALSE
    )
  })

  # update opacity
  observeEvent(input$opacity, {
    req(map_active_df())

    if (app_data$map_drawn == 1) {
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

    if (app_data$map_drawn == 1) {
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
    leaflet::leafletProxy("web_map") %>% leaflet::clearPopups()

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
        leaflet::addPopups(
          event$lng,
          event$lat,
          content,
          layerId = event$id
        )
    })
  })

  # remove legend on selecting a new variable
  observeEvent(map_var(), {
    updateCheckboxInput(
      session,
      "legend",
      value = FALSE
    )

    leaflet::leafletProxy("map") %>%
      leaflet::clearControls() %>%
      leaflet::clearShapes() %>%
      leaflet::clearMarkers()
  })

  # add legend on top of leaflet object
  observe({
    req(map_active_df())

    if ("sf" %in% class(map_active_df()) &
      is.atomic(map_active_df()[[map_var()]]) &
      nrow(map_active_df()) > 0) {

      # Catch GeoPackages with non-spatial tables that GeoPandas has added empty
      # GeometryCollection column to.
      if (any(is.na(sf::st_crs(map_active_df())))) {
        return()
      }

      # make map active layer epsg 4326
      # make this an if statement
      map_df <- try(
        map_active_df() %>%
          sf::st_transform(4326)
      )

      if ("try-error" %in% class(map_df)) {
        return()
      }

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

  # update select input for chart layer
  observe({
    df <- app_data$data_file
    joined_df <- app_data$joined_df
    nm_jdf <- names(joined_df)
    choices <- unique(df$layer_disp_name_idx)
    choices <- c(choices, nm_jdf)
    updateSelectInput(
      session,
      "chart_active_layer",
      choices = choices
    )
  })

  # active df - use this df for rendering chart
  chart_active_df <- reactive({
    req(input$chart_active_layer)

    df <- isolate(app_data$data_file)
    jdf <- isolate(names(app_data$joined_df))

    if (any(jdf == input$chart_active_layer)) {
      chart_active_df <-
        isolate(app_data$joined_df[[input$chart_active_layer]])
    } else {
      chart_active_df <- read_tables(
        df,
        input$chart_active_layer
      )
    }

    chart_active_df
  })

  # histogram variable selection
  hist_choices <- reactive({
    req(chart_active_df())

    tmp_df <- chart_active_df() %>%
      dplyr::select_if(is.numeric)
    choices <- names(tmp_df)
    print(choices)

    choices
  })

  hist_x_axis_vars <-
    mod_single_input_Server(
      id = "hist_x_axis_var",
      s_df = hist_choices
    )

  # scatter plot variable selection
  scatter_choices <- reactive({
    req(chart_active_df())

    tmp_df <- chart_active_df() %>%
      dplyr::select_if(is.numeric)
    choices <- names(tmp_df)
    print(choices)

    choices
  })

  scatter_x_axis_vars <-
    mod_single_input_Server(
      id = "scatter_x_axis_var",
      s_df = scatter_choices
    )

  scatter_y_axis_vars <-
    mod_single_input_Server(
      id = "scatter_y_axis_var",
      s_df = scatter_choices
    )

  # bar plot variables
  col_grouping_var <-
    mod_single_input_Server(
      id = "col_grouping_var",
      s_df = chart_active_df
    )

  # filter out selected grouping variables in list of variables which can be summarised
  col_active_df <- reactive({
    req(chart_active_df(), col_grouping_var())

    tmp_df <- chart_active_df() %>%
      dplyr::select_if(is.numeric)
    choices <- names(tmp_df)
    s_intersect <- intersect(choices, col_grouping_var())
    choices <- choices[!choices %in% s_intersect]

    choices
  })

  col_summarising_var <-
    mod_single_input_Server(
      id = "col_summarising_var",
      s_df = col_active_df
    )

  # perform group by and summarise operation for bar plots
  col_summarised_df <- reactive({
    req(chart_active_df())

    chart_data()

    col_summarising_var <- isolate(col_summarising_var())
    col_grouping_var <- isolate(col_grouping_var())

    if (input$bar_plot_type == "count_records") {
      col_summarising_var <- NULL
    }

    col_group_df <-
      group_by_summarise(
        chart_active_df(),
        col_grouping_var,
        col_summarising_var
      )

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
      chart_active_df <- isolate(chart_active_df())

      if (chart_type == "histogram") {

        binwidth <- isolate(input$binwidth)
        hist_x_var <- isolate(hist_x_axis_vars())
        chart <- make_histogram(
          chart_active_df,
          hist_x_var,
          binwidth,
          x_lab,
          y_lab,
          axis_font_size,
          lab_font_size
        )

      }

      if (chart_type == "scatter") {

          scatter_x_var <- isolate(scatter_x_axis_vars())
          scatter_y_var <- isolate(scatter_y_axis_vars())
          point <- isolate(input$scatter_point_size)
          chart <- make_scatter(
            chart_active_df,
            scatter_x_var,
            scatter_y_var,
            point,
            x_lab,
            y_lab,
            axis_font_size,
            lab_font_size
          )
      }

      if (chart_type == "bar plot") {

        bar_x_var <- isolate(col_summarised_df()[, 1])

        if (bar_plot_type == "count_records") {
          bar_y_var <- isolate(col_summarised_df()[, 2])
        } else if (bar_plot_type == "sum_values") {
          bar_y_var <- isolate(col_summarised_df()[, 3])
        } else if (bar_plot_type == "mean") {
          bar_y_var <- isolate(col_summarised_df()[, 2])
        }

        col_chart_df <- data.frame(bar_x_var, bar_y_var)

        chart <- make_barplot(
          col_chart_df,
          x_lab,
          y_lab,
          axis_font_size,
          lab_font_size
        )
      }

      chart
    },
    height = function() {
      input$chart_height
    },
    bg = "transparent"
  )

  # Admin - data cleaning ---------------------------------------------------

  # reveal FastAPI sync endpoint for authenticated users
  output$sync_endpoint_ui <- renderUI({
    if (!is.null(isolate(token()))) {
      tagList(
        h4("Sync edits to Google Cloud Storage"),

        textInput(
          inputId = "sync_endpoint",
          label = "Endpoint for sync API"
        ),

        actionButton(
          "sync_edits",
          "sync edits",
          class = "btn-primary m-2"
        ),
        actionButton(
          "refresh_data",
          "refresh data",
          class = "btn-primary m-2"
        )
      )

    } else {
      return()
    }
  })

  # user uploaded files for data cleaning / editing
  # return table of files and file paths of data loaded to the server
  upload_edit_file <- mod_get_layers_Server(id = "edit_data")

  # user can download a file to clean  / edit from Google Cloud Storage
  # get list of GCS Buckets associated with a project ID
  observeEvent(input$admin_list_google_files, {
    req(token())
    req(input$admin_gcs_project_id)

    buckets <- try(
      list_gcs_buckets(
        token(),
        input$admin_gcs_project_id
      )
    )

    if ("try-error" %in% buckets) {
      shiny::showNotification(
        "Error listing buckets Google Cloud Storage",
        type = "error",
        duration = 5
      )
      return()
    }

    if ("no items returned" %in% buckets | is.null(buckets)) {
      shiny::showNotification(
        "No buckets available in Google Cloud Storage",
        type = "error",
        duration = 5
      )
    } else {
      # update admin_buckets element of app_data with list of buckets in GCS project
      app_data$admin_buckets <- buckets
    }
  })

  # update selectInput with list GCS buckets in GCS project
  observe({
    app_data$admin_buckets

    if (length(app_data$admin_buckets) > 0 & !"no items returned" %in% app_data$admin_buckets) {
      updateSelectInput(
        session,
        "admin_gcs_bucket_name",
        choices = app_data$admin_buckets
      )
    }
  })

  # get list of objects in selected GCS bucket
  observe({
    req(input$admin_gcs_bucket_name)

    items <- list_gcs_bucket_objects(
      token(),
      input$admin_gcs_bucket_name
    )

    if ("no items returned" %in% items | is.null(items)) {
      shiny::showNotification(
        "No GeoPackages returned from Google Cloud Storage bucket query",
        type = "error",
        duration = 5
      )
      app_data$admin_items <- NULL
    } else {
      # update admin_items object of app_data with list of GeoPackages in GCS bucket
      app_data$admin_items <- items
    }
  })

  # update selectInput with list of objects in GCS bucket
  observe({
    app_data$admin_items

    if (length(app_data$admin_items) > 0 & !"no items returned" %in% app_data$admin_items) {
      updateSelectInput(
        session,
        "admin_gcs_bucket_objects",
        choices = app_data$admin_items
      )
    } else {
      updateSelectInput(
        session,
        "admin_gcs_bucket_objects",
        choices = ""
      )
    }
  })

  # add user selected GCS object to list of layers
  # add info for GeoPackage downloaded from Google Cloud Storage to app_data$data_file and unpack layers in GeoPackage
  observeEvent(input$admin_get_objects, {
    req(input$admin_gcs_bucket_objects)

    selected_gcs_object <- input$admin_gcs_bucket_objects

    gcs_gpkg <- NULL
    gcs_gpkg <- try(
      # returns a 2-element list - temporary path to GeoPackage and name of GeoPackage to display
      get_gcs_object(
        token(),
        input$admin_gcs_bucket_name,
        selected_gcs_object
      )
    )

    f_lyrs <- NULL

    if (!"try-error" %in% class(gcs_gpkg) & !is.null(gcs_gpkg) & !any(stringr::str_detect(gcs_gpkg, "cannot load GeoPackage from Google Cloud Storage"))) {
      f_lyrs <- try(
        # list layers and temporary path to GeoPackage downloaded from GCS
        purrr::map2(
          gcs_gpkg$f_path,
          gcs_gpkg$f_name,
          list_layers
        ) %>%
          dplyr::bind_rows()
      )

      # keep track of GeoPackage being edited (f_name) and its GCS Bucket (admin_current_bucket) for syncing edits back to files on GCS
      app_data$admin_fname <- gcs_gpkg$f_name # name of GeoPacakge being edited
      app_data$admin_current_bucket <- input$admin_gcs_bucket_name # name of GCS bucket where GeoPackage being edited is stored

      isolate({
        # clear previously uploaded data
        app_data$edit_data_file <- data.frame()
        df <- dplyr::bind_rows(
          app_data$edit_data_file,
          f_lyrs
        )
        # unique number id next to each layer to catch uploads of tables with same name
        rows <- nrow(df)
        row_idx <- 1:rows
        df$layer_disp_name_idx <-
          paste0(df$layer_disp_name, "_", row_idx, sep = "")
        app_data$edit_data_file <- df

        # create new log for edits
        # log file for any errors
        app_data$edit_log <- NULL
        log <-
          fs::file_temp(
            pattern = "log",
            tmp_dir = tempdir(),
            ext = "txt"
          )
        app_data$edit_log <- log
      })

      # reset map zoom to new data's extent
      app_data$map_edits_zoom <- 0
    }
  })

  # handle file uploads from user's local machine
  # add info for GeoPackage uploaded by user to app_data$data_file and unpack layers in GeoPackage
  observe({
    req(upload_edit_file())

    upload_edit_file <- isolate(upload_edit_file())
    isolate({
      # clear previously uploaded data
      app_data$edit_data_file <- data.frame()
      df <- dplyr::bind_rows(app_data$edit_data_file, upload_edit_file)
      # unique number id next to each layer to catch uploads of tables with same name
      rows <- nrow(df)
      row_idx <- 1:rows
      df$layer_disp_name_idx <-
        paste0(df$layer_disp_name, "_", row_idx, sep = "")
      app_data$edit_data_file <- df

      # create new log for edits
      # log file for any errors
      app_data$edit_log <- NULL
      log <-
        fs::file_temp(
          pattern = "log",
          tmp_dir = tempdir(),
          ext = "txt"
        )
      app_data$edit_log <- log
    })

    # reset map zoom to new data's extent
    app_data$map_edits_zoom <- 0
  })

  # select one layer to edit
  observe({
    df <- app_data$edit_data_file
    choices <- unique(df$layer_disp_name_idx)
    updateSelectInput(
      session,
      "edit_layer",
      choices = choices
    )
  })

  # edit df - read the layer to edit into a (sf) data frame to edit
  edit_df <- reactive({
    req(input$edit_layer)
    req(app_data$flush_deletes) # update the data frame after features / rows are deleted
    req(app_data$flush_edits) # update the data frame after features / cells are edited
    req(app_data$flush_geometry_edits) # update the data frame after feature geometries are edited

    # get info on path to GeoPackage storing layer to edit
    df <- try(
      isolate(app_data$edit_data_file)
    )
    if ("try-error" %in% class(df)) {
      shiny::showNotification(
        "Data could not be loaded",
        type = "error",
        duration = 5
      )
      return()
    }

    # read layer / table to edit into a (sf) data frame
    edit_df <- try(
      read_tables(
        df,
        input$edit_layer
      )
    )

    if ("try-error" %in% class(edit_df)) {
      shiny::showNotification(
        "Data could not be loaded",
        type = "error",
        duration = 5
      )
      return()
    }

    # check if layer to be edited is spatial
    # if not spatial drop geometry and convert to data frame
    # if spatial transform to 4326 for leaflet
    edit_layer_crs <- NA

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
    } else if (is.na(edit_layer_crs)) {
      edit_df <- edit_df
    }

    edit_df
  })

  # render data frame to edit as editable data table
  mod_render_dt_Server(
    id = "edit_data_dt",
    dt = edit_df,
    editable = TRUE
  )

  # add editing layer to map
  # edit_leafmap is the web map object for editing feature geometries
  output$edit_leafmap <- leaflet::renderLeaflet({
    base_map <- leaflet::leaflet() %>%
      leaflet::addTiles(group = "OSM (default)") %>%
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldImagery,
        options = leaflet::providerTileOptions(maxZoom = 17),
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

  # add spatial data to edit to edit_leafmap
  observe({
    req(edit_df())

    # add layerID to edit_df - this to identify features associated with user click events
    edit_df <- edit_df()
    if (nrow(edit_df) > 0) {
      edit_df$layer_id <- as.character(1:nrow(edit_df))
    }

    edit_layer <- isolate(input$edit_layer)

    # only render map if it is spatial (of class sf)
    if ("sf" %in% class(edit_df) &
      nrow(edit_df) > 0 &
      input$edit_data_view == "edit_map" &
      app_data$map_edits_zoom == 0) {
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
      app_data$map_edits_zoom > 0) {
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

    # if a user has not clicked on a marker or object leave event as null
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

    # filter the feature associated with the user click
    event <- event$id
    app_data$event_tmp <- as.numeric(event) # keep track of the user's click event
    edit_df <- isolate(edit_df())
    feature <- edit_df[as.numeric(event), ]
    feature$layer_id <- 1

    # get bounding box for the extent of the feature clicked by the user
    bbox <- sf::st_bbox(feature) %>%
      as.vector()

    # add editing feature to edit_zoommap
    # edit_zoommap is a popup map to display the feature to edit and allow interactive editing
    output$edit_zoommap <- leaflet::renderLeaflet({
      zoom_map <- leaflet::leaflet() %>%
        leaflet::addTiles(group = "OSM (default)") %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          options = leaflet::providerTileOptions(maxZoom = 17),
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
        leaflet::leafletOutput("edit_zoommap"),
        modalButton("close"),
        easyClose = TRUE,
        footer = NULL
      )
    )
  })

  # Listen for editing feature geometry event
  observeEvent(input$edit_zoommap_draw_edited_features, {
    req(edit_df())
    edits <- input$edit_zoommap_draw_edited_features
    # convert return from leaflet.draw to json so it can be read by st_read
    edits_to_json <- jsonlite::toJSON(
      edits,
      auto_unbox = TRUE,
      force = TRUE,
      digits = NA
    )
    edits_to_sf <- sf::st_read(edits_to_json)

    # data frame of layer currently being edited
    edit_df <- edit_df()
    colnames <- colnames(edit_df)
    # row in data frame associated with the feature being edited
    row_to_edit <- app_data$event_tmp

    # replace old geometry with new geometry generated by the user
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

    # write edited layer back to GeoPackage
    write_tables(
      edit_df,
      isolate(app_data$edit_data_file),
      input$edit_layer
    )
    app_data$map_edits_zoom <- app_data$map_edits_zoom + 1
    app_data$flush_geometry_edits <- app_data$flush_geometry_edits + 1 # trigger reload of layer from GeoPackage with newly edited features
    app_data$event_tmp <- NULL # reset event_tmp to NULL ready for next user click
  })

  # Listen for users deleting features events
  observeEvent(input$edit_zoommap_draw_deleted_features, {
    req(edit_df())
    # convert return from leaflet.draw to json so it can be read by st_read
    deletes <- input$edit_zoommap_draw_deleted_features
    deletes_to_json <- jsonlite::toJSON(
      deletes,
      auto_unbox = TRUE,
      force = TRUE,
      digits = NA
    )
    deletes_to_sf <- sf::st_read(deletes_to_json)

    # get data frame to delete feature from
    edit_df <- edit_df()
    row_to_delete <- app_data$event_tmp

    if (nrow(deletes_to_sf) > 0) {
      edit_df <- edit_df %>%
        dplyr::filter(dplyr::row_number() != row_to_delete)
      # write layer with feature deleted to GeoPackage
      write_tables(
        edit_df,
        isolate(app_data$edit_data_file),
        input$edit_layer
      )
      app_data$flush_geometry_edits <- app_data$flush_geometry_edits + 1 # trigger reload of layer from GeoPackage with feature deleted
    }

    app_data$map_edits_zoom <- app_data$map_edits_zoom + 1
    app_data$event_tmp <- NULL # reset event_tmp to NULL ready for next user click
  })

  # waiter object to display while deleting features
  delete_waiter <- waiter::Waiter$new(
    html = delete_screen,
    color = "rgba(44,62,80,.6)"
  )

  # Table-based editing of layer in a Data Table
  # delete selected rows from GeoPackage
  observeEvent(input$delete_records, {
    delete_waiter$show()
    selected_rows <- input$`edit_data_dt-data_table_rows_selected`
    id_str <- input$row_id
    edit_df_colnames <- colnames(edit_df())

    # layer that is edited prior to any deletions - keep to preserve row indexes
    pre_edit_df <- edit_df()

    # check if there is a unique column id that can be used to propagate deletes through related tables (e.g. suffix == "_id")
    # this would be better handled using a QGS file???
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

    shinyFeedback::feedbackWarning(
      "row_id",
      is.null(id_col),
      "Cannot uniquely identify id column"
    )

    if (length(selected_rows) > 0 & !is.null(id_col)) {
      for (i in selected_rows) {
        # this is unique row id that defines row to be deleted
        id_to_delete <- pre_edit_df[i, ][[id_col]]

        # only delete rows if selected row-column is not null or NA
        if (!is.na(id_to_delete) & !is.null(id_to_delete)) {
          # get layer to delete row from
          edit_gpkg <- app_data$edit_data_file
          layers <- unique(edit_gpkg$layer_disp_name_idx)
          # iterate over layers, delete row, and sync changes to GeoPackage
          for (ii in layers) {
            tmp_edit_df <- read_tables(
              isolate(app_data$edit_data_file),
              ii
            )
            tmp_edit_df_colnames <- colnames(tmp_edit_df)
            tmp_edit_df_id_col <- stringr::str_detect(tmp_edit_df_colnames, id_str)
            tmp_edit_df_id_col <- tmp_edit_df_colnames[tmp_edit_df_id_col]

            if (length(tmp_edit_df_id_col) > 1) {
              tmp_edit_df_id_col <- NULL
              # write delete fail error message to log
              readr::write_lines(
                paste0("cannot delete row in layer: ", ii, " - cannot uniquely identify id column"),
                app_data$edit_log,
                sep = "\n",
                na = "NA",
                append = TRUE
              )
            } else if (length(tmp_edit_df_id_col) == 0) {
              tmp_edit_df_id_col <- NULL
              # write delete fail error message to log
              readr::write_lines(
                paste0("cannot delete row in layer: ", ii, " - id column missing"),
                app_data$edit_log,
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
                  # write layer to GeoPackage with row deleted
                  write_tables(
                    tmp_edit_df,
                    isolate(app_data$edit_data_file),
                    ii
                  )
                  paste0("deleted row-id: ", id_to_delete, "from layer: ", ii)
                }
              )
              # write record of row deletion in log
              readr::write_lines(
                delete_message,
                app_data$edit_log,
                sep = "\n",
                na = "NA",
                append = TRUE
              )
            }
          }
        }
      }
    }
    app_data$flush_deletes <- app_data$flush_deletes + 1 # trigger reload of layer from GeoPackage with row deleted
    delete_waiter$hide()
  })

  # watier object to display while applying edits
  edit_waiter <- waiter::Waiter$new(
    html = edit_screen,
    color = "rgba(44,62,80,.6)"
  )

  # Table-based editing of layer in a Data Table
  # listen for user edits to records in Data Table
  observeEvent(input$`edit_data_dt-data_table_cell_edit`, {
    tmp_edits <- app_data$tmp_edits
    edited_rows <- input$`edit_data_dt-data_table_cell_edit`
    tmp_edits <- dplyr::bind_rows(
      tmp_edits,
      edited_rows
    )
    app_data$tmp_edits <- tmp_edits # temporary record of user edits to records in Data Table
  })

  # apply edits and updating GeoPackage
  observeEvent(input$save_edits, {

    # layer that is edited prior to any edits
    df_to_edit <- edit_df()

    #  drop sticky geometry column if layer is sf object
    if ("sf" %in% class(df_to_edit)) {
      df_to_edit_not_sf <- df_to_edit %>%
        sf::st_drop_geometry()
    } else {
      df_to_edit_not_sf <- df_to_edit
    }

    # user generated edits to apply to data frame
    tmp_edits <- app_data$tmp_edits

    if (nrow(tmp_edits) > 0) {
      edit_waiter$show()
      edits <- edit_data_frame(
        tmp_edits,
        df_to_edit,
        df_to_edit_not_sf,
        input$edit_layer
      )
      df_to_edit <- edits[[1]]
      # update GeoPackage with user generated edits to records in layer
      write_tables(
        df_to_edit,
        isolate(app_data$edit_data_file),
        input$edit_layer
      )
      edits_log <- edits[[2]]

      # update log with status of applying user generated edits
      for (i in edits_log) {
        readr::write_lines(
          i,
          app_data$edit_log,
          sep = "\n",
          na = "NA",
          append = TRUE
        )
      }
      edit_waiter$hide()
    }

    # reset edits object to empty after applying them GeoPackage
    app_data$tmp_edits <- data.frame()
    app_data$flush_edits <- app_data$flush_edits + 1 # trigger reload of layer with user generated edits applied
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

    # user generated edits to apply to data frame
    tmp_edits <- app_data$tmp_edits

    if (nrow(tmp_edits) > 0) {
      edit_waiter$show()
      edits <- edit_data_frame(
        tmp_edits,
        df_to_edit,
        df_to_edit_not_sf,
        input$edit_layer
      )
      df_to_edit <- edits[[1]]
      write_tables(
        df_to_edit,
        isolate(app_data$edit_data_file),
        input$edit_layer
      )
      edits_log <- edits[[2]]
      for (i in edits_log) {
        readr::write_lines(
          i,
          app_data$edit_log,
          sep = "\n",
          na = "NA",
          append = TRUE
        )
      }
      edit_waiter$hide()
    }

    # reset edits object after applying edits to the GeoPackage
    app_data$tmp_edits <- data.frame()
    app_data$flush_edits <- app_data$flush_edits + 1
  })

  # sync edited GeoPackage back to Google Cloud Storage
  # update selectInput with list of Google Cloud Storage buckets
  observe({
    app_data$admin_buckets

    if (length(app_data$admin_buckets) > 0 & !"no items returned" %in% app_data$admin_buckets) {
      updateSelectInput(
        session,
        "sync_gcs_bucket_name",
        choices = app_data$admin_buckets
      )
    }
  })

  # waiter to display while syncing edits back to Google Cloud Storage
  sync_edit_waiter <- waiter::Waiter$new(
    html = edit_screen,
    color = "rgba(44,62,80,.6)"
  )

  observeEvent(input$sync_edits, {
    req(input$sync_endpoint)
    req(edit_df())

    sync_edit_waiter$show()
    # get file name of GeoPackage to sync to server
    fname <- app_data$admin_fname

    # FastAPI endpoint for sync edits operations
    endpoint <- input$sync_endpoint

    # rename local file in Shiny App to match file in Google Cloud Storage
    path_file_to_post <- app_data$edit_data_file
    path_file_to_post <- path_file_to_post$file_path[1]
    tmp_dir <- dirname(path_file_to_post)
    sync_tmp_path <- paste0(tmp_dir, "/", fname)
    fs::file_copy(
      path_file_to_post,
      sync_tmp_path,
      overwrite = TRUE
    )

    # post GeoPackage with edits applied to FastAPI endpoint
    req <- try(httr::POST(
      url = endpoint,
      config = httr::config(token = token()),
      body = list(file = httr::upload_file(sync_tmp_path))
    ))

    if ("try-error" %in% class(req)) {
      shiny::showNotification(
        paste0("Edits could not be synced"),
        duration = 5
      )
    } else {
      shiny::showNotification(
        paste0("Sync response status: ", req$status_code),
        duration = 5
      )
    }

    sync_edit_waiter$hide()
  })

  # update data in app with latest GeoPackage stored on Google Cloud Storage
  observeEvent(input$refresh_data, {
    selected_gcs_object <- app_data$admin_fname
    admin_current_bucket <- app_data$admin_current_bucket

    gcs_gpkg <- NULL
    gcs_gpkg <- try(
      # returns a 2-element list - temporary path to GeoPackage and name of GeoPackage to display
      get_gcs_object(
        token(),
        admin_current_bucket,
        selected_gcs_object
      )
    )

    f_lyrs <- NULL

    if (!"try-error" %in% class(gcs_gpkg) & !is.null(gcs_gpkg) & !any(stringr::str_detect(gcs_gpkg, "cannot load GeoPackage from Google Cloud Storage"))) {
      f_lyrs <- tryCatch(
        error = function(cnd) NULL,
        purrr::map2(
          gcs_gpkg$f_path,
          gcs_gpkg$f_name,
          list_layers
        ) %>%
          dplyr::bind_rows()
      )

      # keep track of GeoPackage being edited (f_name) and its GCS Bucket (admin_current_bucket) for syncing edits back to files on GCS
      app_data$admin_fname <- gcs_gpkg$f_name
      app_data$admin_current_bucket <- admin_current_bucket

      isolate({
        # clear previously uploaded data
        app_data$edit_data_file <- data.frame()
        df <- dplyr::bind_rows(app_data$edit_data_file, f_lyrs)
        # unique number id next to each layer to catch uploads of tables with same name
        rows <- nrow(df)
        row_idx <- 1:rows
        df$layer_disp_name_idx <-
          paste0(df$layer_disp_name, "_", row_idx, sep = "")
        app_data$edit_data_file <- df

        # create new log for edits
        # log file for any errors
        app_data$edit_log <- NULL
        log <-
          fs::file_temp(
            pattern = "log",
            tmp_dir = tempdir(),
            ext = "txt"
          )
        app_data$edit_log <- log
      })

      # reset map zoom to new data's extent
      app_data$map_edits_zoom <- 0
    }

  })

  # download edited data as a zip file
  output$download_edits <- downloadHandler(
    filename = function() {
      req(nrow(app_data$edit_data_file) >= 1)

      paste("edited_", dt(), ".zip", sep = "")
    },
    content = function(file) {
      req(nrow(app_data$edit_data_file) >= 1)

      dpath <- app_data$edit_data_file
      # row 1 should be the path to the GeoPackage that is being edited as a user can only load and edit one GeoPackage at a time
      dpath <- dpath[1, ][["file_path"]]
      log_path <- app_data$edit_log
      zip(
        zipfile = file,
        files = c(dpath, log_path),
        flags = "-r9Xj"
      )
    },
    contentType = "application/zip"
  )
}
