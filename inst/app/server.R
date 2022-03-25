
# max size of local files that can be uploaded
options(shiny.maxRequestSize = 100000 * 1024^2)

app_server <- function(input, output, session) {

  # hold the loading screen for 2 seconds
  Sys.sleep(2)

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

  observeEvent(input$`grouping_var-multiple_input` , {
    updateTabsetPanel(inputId = "data_tables",  selected = "Data: Summary")
  })

  observeEvent(input$active_layer, {
    updateTabsetPanel(inputId = "data_tables",  selected = "Data: Raw")
  })

  observeEvent(input$table_filter, {
    updateTabsetPanel(inputId = "data_tables",  selected = "Data: Raw")
  })

  observeEvent(input$filter, {
    updateTabsetPanel(inputId = "data_tables",  selected = "Data: Raw")
  })

  observeEvent(input$table_mutate, {
    updateTabsetPanel(inputId = "data_tables",  selected = "Data: Raw")
  })

  observeEvent(input$add_column, {
    updateTabsetPanel(inputId = "data_tables",  selected = "Data: Raw")
  })

  observeEvent(input$table_join_button, {
    updateTabsetPanel(inputId = "data_tables",  selected = "Data: Raw")
  })

  observeEvent(input$spatial_join_button, {
    updateTabsetPanel(inputId = "data_tables",  selected = "Data: Raw")
  })

  # Waiting screens ---------------------------------------------------------

  # waiter for downloading projects
  project_waiter <- waiter::Waiter$new(
    html = project_screen,
    color = "rgba(89,49,150,.6)"
  )

  # waiter for downloading files
  download_waiter <- waiter::Waiter$new(
    html = download_screen,
    color = "rgba(89,49,150,.6)"
  )

  # sync forms with database / template
  sync_waiter <- waiter::Waiter$new(
    html = sync_screen,
    color = "rgba(89,49,150,.6)"
  )

  # waiting screen for drawing maps
  map_waiter <- waiter::Waiter$new(
    html = map_screen,
    color = "rgba(89,49,150,.6)"
  )

  # waiting screen for logging in
  login_waiter <- waiter::Waiter$new(
    html = login_screen,
    color = "rgba(89,49,150,.6)"
  )

  # waiting screen for joining tables
  join_waiter <- waiter::Waiter$new(
    html = join_screen,
    color = "rgba(89,49,150,.6)"
  )

  spatial_join_waiter <- waiter::Waiter$new(
    html = join_screen,
    color = "rgba(89,49,150,.6)"
  )

  # App data ----------------------------------------------------------------

  # object storing data to pass between reactive objects during app's execution
  app_data <-
    reactiveValues(
      data_file = data.frame(), # dataframe of listing layers and temporary locations of data a user has uploaded
      map_drawn = 0, # indicator for state of map (if 0, zoom to data's bbox)
      joined_df = list(), # list of layers created through joins, spatial joins, or filtering rows
      flush_add_column = 0, # trigger re-render of active layer in data table after adding column
      qfieldcloud_token = NULL, # qfieldcloud token obtained from successful login
      qfieldcloud_projects = NULL, # dataframe of qfieldcloud projects and project ids
      qfieldcloud_files = NULL, # dataframe of qfieldcloud project files and project ids
      buckets = NULL, # list of buckets in users GCS project
      items = NULL, # items in GCS bucket
      layers_df = NULL, # dataframe of layers in the app
      qfieldcloud_token = NULL, # QFieldCloud token
      qfieldcloud_url = NULL # qfieldcloud url - set on successful login
    )


  # Data tab ----------------------------------------------------------------

  # render layers as data table in UI
  # display of what layers the user has loaded into the app
  layers_df <- reactive({

    layers_df <- app_data$data_file

    if (is.null(layers_df)) {
      return()
    }

    if (nrow(layers_df) <= 0){
      return()
    }

    layers_df <- layers_df %>%
      dplyr::select(c("layers", "layer_disp_name", "file_type", "source", "layer_disp_name_idx"))

    joined_df <- app_data$joined_df

    if (length(joined_df) > 0) {
      tmp_layer_names <- names(joined_df)
      tmp_layers <- data.frame(layers = tmp_layer_names)
      tmp_layers$layer_disp_name <- "user generated"
      tmp_layers$file_type <- "app"
      tmp_layers$source <- "user generated"
      tmp_layers$layer_disp_name_idx <- "user generated"

      layers_df <- layers_df %>%
        dplyr::bind_rows(tmp_layers) %>%
        dplyr::select(c("layers", "layer_disp_name", "source", "layer_disp_name_idx")) %>%
        dplyr::rename(
          layer = layers,
          `sourcefile` = layer_disp_name,
          source = source,
          `layer display name` = layer_disp_name_idx
        )
    } else {
      layers_df <- layers_df %>%
        dplyr::select(c("layers", "layer_disp_name", "source", "layer_disp_name_idx")) %>%
        dplyr::rename(
          layer = layers,
          `sourcefile` = layer_disp_name,
          source = source,
          `layer display name` = layer_disp_name_idx
        )
    }

    layers_df
  })

  mod_render_dt_Server(
    id = "app_layers",
    dt = layers_df,
    editable = FALSE
  )

  # Data Sync ---------------------------------------------------------------

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

  # Data Upload -------------------------------------------------------------

  # user uploaded files
  # return table of files and file paths of data loaded to the server
  upload_file <- mod_get_layers_Server(id = "user_data")

  # update app_data data_file object of layers a user can select as active layer with user uploaded data
  observe({
    req(upload_file())

    tryCatch(
      error = function(cnd) {
        showNotification("Error uploading file. Check it is a valid GeoPackage.", type = "error")
        return()
      },
      {
        upload_file <- isolate(upload_file())

        upload_file$source <- "Local file"

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

      }
    )
  })

  # QFieldCloud data --------------------------------------------------------

  # token is TRUE if the user is logged in successfully
  # get QFieldCloud token
  observeEvent(input$login, {
    username <- input$qfieldcloud_username
    password <- input$qfieldcloud_password
    endpoint <- input$qfieldcloud_url

    login_waiter$show()

    token <- qfieldcloudR::qfieldcloud_login(
      username,
      password,
      endpoint
    )

    if (token$status == "success") {
      app_data$qfieldcloud_token <- token$token

      login_message <- paste0("logged in as ", username)
      app_data$qfieldcloud_url <- input$qfieldcloud_url

      output$login_status <- renderUI({
        tags$p(login_message)
      })
    } else {
      app_data$qfieldcloud_token <- NULL
      login_message <-
        paste0("login failed - check user email and password.")
      output$login_status <- renderUI({
        tags$p(login_message, style="color:red;")
      })
    }

    login_waiter$hide()
  })

  # get list of QFieldCloud projects
  observe({
    req(app_data$qfieldcloud_token)

    app_data$qfieldcloud_token

    project_waiter$show()

    tryCatch(
      error = function(cnd) {
        showNotification("Could not load projects.", type = "error")
      },
      {
        qfieldcloud_projects <- qfieldcloudR::get_qfieldcloud_projects(
          app_data$qfieldcloud_token,
          app_data$qfieldcloud_url
        )

        app_data$qfieldcloud_projects <- qfieldcloud_projects
      }
    )

    project_waiter$hide()

  })

  # update select input with list of QFieldCloud projects
  observe({
    req(app_data$qfieldcloud_projects)
    req(app_data$qfieldcloud_token)

    app_data$qfieldcloud_projects

    projects <- app_data$qfieldcloud_projects
    projects <- projects$name

    if (!is.null(app_data$qfieldcloud_projects) & nrow((app_data$qfieldcloud_projects)) > 0) {
      updateSelectInput(
        session,
        "qfieldcloud_projects",
        choices = projects
      )
    }
  })

  # update select input with list of QFieldCloud project GeoPackages
  observe({
    req(input$qfieldcloud_projects)

    input$qfieldcloud_projects

    download_waiter$show()

    tryCatch(
      error = function(cnd) {
        shiny::showNotification("Failed to download project files - empty project?", type = "error")
      },
      {
        projects <- app_data$qfieldcloud_projects %>%
          dplyr::filter(name == input$qfieldcloud_projects)

        project_id <- projects[, 2]

        files <- qfieldcloudR::get_qfieldcloud_files(
          app_data$qfieldcloud_token,
          app_data$qfieldcloud_url,
          project_id
        )

        app_data$qfieldcloud_files <- files

        if (is.null(files) | nrow(files) <= 0) {
          updateSelectInput(
            session,
            "qfieldcloud_gpkg",
            choices = "",
            selected = ""
          )
        } else {
          updateSelectInput(
            session,
            "qfieldcloud_gpkg",
            choices = files$name
          )
        }

      }
    )

    download_waiter$hide()
  })

  # clean up select inputs on logout
  observe({
    if (is.null(app_data$qfieldcloud_token)) {
      updateSelectInput(
        session,
        "qfieldcloud_projects",
        choices = "",
        selected = ""
      )

      updateSelectInput(
        session,
        "qfieldcloud_gpkg",
        choices = "",
        selected = ""
      )
    }
  })

  # add user selected QFieldCloud file to list of layers
  # write GeoPackage to app_data$data_file and unpack layers in GeoPackage
  observeEvent(input$get_qfieldcloud_gpkg, {
    req(input$qfieldcloud_gpkg)
    req(input$qfieldcloud_projects)
    req(app_data$qfieldcloud_token)

    filename <- input$qfieldcloud_gpkg

    download_waiter$show()

    tryCatch(
      error = function(cnd) {
        shiny::showNotification("Failed to download project file.", type = "error")
      },
      {

        projects <- app_data$qfieldcloud_projects %>%
          dplyr::filter(name == input$qfieldcloud_projects)

        project_id <- projects[, 2]

        qfieldcloud_gpkg <-  qfieldcloudR::get_qfieldcloud_file(
          app_data$qfieldcloud_token,
          app_data$qfieldcloud_url,
          project_id,
          filename
        )

        f_lyrs <- purrr::map2(
          qfieldcloud_gpkg$tmp_file,
          qfieldcloud_gpkg$filename,
          list_layers
        ) %>%
          dplyr::bind_rows()

        if (!is.null(f_lyrs)) {
          f_lyrs$source <- "QFieldCloud"
        }

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

      }
    )

    download_waiter$hide()

  })

  # Active layer ------------------------------------------------------------

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

  # active layer - layer to display in data table
  active_df <- reactive({
    req(input$active_layer)

    # update table after add column operation
    update_table <- add_column_count()

    df <- isolate(app_data$data_file)
    jdf <- isolate(names(app_data$joined_df))

    if (any(jdf == input$active_layer)) {
      active_df <- try(app_data$joined_df[[input$active_layer]])
    } else {
      active_df <- try(read_tables(df, input$active_layer))
    }

    if ("try-error" %in% class(active_df)) {
      showNotification("Could not load active layer.", type = "error")
      return()
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

    summarised_df <-try(
        group_by_summarise(
          active_df(),
          grouping_vars(),
          summarising_vars()
        )
      )

    if ("try-error" %in% class(summarised_df)) {
      showNotification("Failed to perform group-by and summarise.", type = "error")
      return()
    }

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

  # non-spatial (key-based) joins
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
      left_df <- try(isolate(app_data$joined_df[[input$table_left]]))
    } else {
      left_df <- try(read_tables(df, input$table_left))
    }

    if ("try-error" %in% left_df) {
      showNotification("error loading left layer for join.", type = "error")
      return()
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
      right_df <- try(isolate(app_data$joined_df[[input$table_right]]))
    } else {
      right_df <- try(read_tables(df, input$table_right))
    }

    if ("try-error" %in% right_df) {
      showNotification("error loading right layer for join.", type = "error")
      return()
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

  # join left table to right table
  observeEvent(input$table_join_button, {
    req(left_df(), right_df(), input$key_join_type, req(f_key(), req(p_key())))

    join_waiter$show()

    tryCatch(
      error = function(cnd) {
        showNotification("Error joining layers.", type = "error")
        return()
      },
      {
        joined_table <-
          join_tables(
            left_df(),
            right_df(),
            input$key_join_type,
            p_key(),
            f_key()
          )

        app_data$joined_df[[input$join_tbl_name]] <- joined_table
      }
    )

    join_waiter$hide()

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
      left_df <- try(read_tables(df, input$spatial_table_left))
    }

    if ("try-error" %in% left_df) {
      showNotification("Error loading left layer.", type = "error")
      return()
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
      right_df <- try(read_tables(df,input$spatial_table_right))
    }

    if ("try-error" %in% right_df) {
      showNotification("Error loading right layer.", type = "error")
      return()
    }

    shinyFeedback::feedbackWarning(
      "spatial_table_right",
      !("sf" %in% class(right_df)),
      "Not a spatial layer"
    )

    right_df
  })

  # join left table to right table
  observeEvent(input$spatial_join_button, {
    req(
      spatial_left_df(),
      spatial_right_df(),
      "sf" %in% class(spatial_left_df()),
      "sf" %in% class(spatial_right_df())
    )

    spatial_join_waiter$show()

    tryCatch(
      error = function(cnd) {
        shiny::showNotification("Error performing spatial join. Check both tables are spatial with geometry columns.", type = "error")
      },
      {
        left_df <- spatial_left_df() %>%
          dplyr::rename_with(tolower)

        right_df <- spatial_right_df() %>%
          dplyr::rename_with(tolower)

        # make geometry column name geometry
        sf::st_geometry(left_df) <- "geometry"
        sf::st_geometry(right_df) <- "geometry"

        # catch common names across left and right tables and append '_y' to duplicate colnames
        left_names <- names(left_df)
        right_names <- names(right_df)

        new_names <- c()

        for (i in right_names) {
          if (i %in% left_names) {
            if (i == "geometry") {
              new_names <- c(new_names, i)
            } else {
              tmp <- paste0(i, "_y")
              new_names <- c(new_names, tmp)
            }
          } else{
            new_names <- c(new_names, i)
          }
        }

        names(right_df) <- new_names

        make_spatial_db_table(
          con,
          left_df,
          "left_df",
          right_df,
          "right_df"
        )

        joined_table <- db_spatial_join_tables(
          left_names,
          new_names,
          "left_df",
          "right_df"
        )

        app_data$joined_df[[input$spjoin_tbl_name]] <- joined_table

      }

    )

    spatial_join_waiter$hide()

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
      filter_df <- try(read_tables(df, input$table_filter))
    }

    if ("try-error" %in% filter_df) {
      showNotification("Failed to load layer for filtering rows.", type = "error")
      return()
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
        "Error filtering rows - no output layer name",
        type = "error",
        duration = 5
      )
      removeModal()

      return()
    }

    if (length(input$filter_tbl_name) < 1) {
      shiny::showNotification(
        "Error filtering rows - no output layer name",
        type = "error",
        duration = 5
      )
      removeModal()

      return()
    }

    if (is.null(input$filter_tbl_name)) {
      shiny::showNotification(
        "Error filtering rows - no output layer name",
        type = "error",
        duration = 5
      )
      removeModal()

      return()
    }

    if (is.character(filter_out) & ("filter error" %in% filter_out)) {
      shiny::showNotification(
        "Error filtering rows",
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
        "Filter complete - new table in active layers",
        type = "message",
        duration = 5
      )
      removeModal()
    } else {
      shiny::showNotification(
        "Error filtering rows - check condition and column names",
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
      mutate_df <- try(read_tables(df, input$table_mutate))
    }

    if ("try-error" %in% class(mutate_df)) {
      showNotification("Failed to read layer to add column to.", type = "error")
      return()
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
        "Error adding column - no column name",
        type = "error",
        duration = 5
      )
      removeModal()

      return()
    }

    if (length(input$col_name) < 1) {
      shiny::showNotification(
        "Error adding column - no column name",
        type = "error",
        duration = 5
      )
      removeModal()

      return()
    }

    if (is.null(input$col_name)) {
      shiny::showNotification(
        "Error adding column - no column name",
        type = "error",
        duration = 5
      )
      removeModal()

      return()
    }

    if (is.character(mutate_out) & ("mutate error" %in% mutate_out)) {
      shiny::showNotification(
        "Error adding column - check condition",
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

    app_data$map_drawn <- 0

    map_active_df
  })

  map_var <-
    mod_single_input_Server(
      id = "map_var",
      s_df = map_active_df
    )

  observe({
    req(map_var())

    shinyFeedback::feedbackWarning(
      "map_var-single_input",
      (map_var() == "geometry" | map_var() == "geom"),
      "Cannot map geometry column to colour palette."
    )

  })

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

  observeEvent(input$map_colour, {
    colour_ramp <- make_colour_ramp(input$map_colour)

    output$colour_ramp <- renderPlot({
      colour_ramp
    })

  })



  # update line colour
  observeEvent(input$map_line_colour, {
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

  # update line width
  observeEvent(input$map_line_width, {
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

    leaflet::leafletProxy("web_map") %>%
      leaflet::clearControls() %>%
      leaflet::clearShapes() %>%
      leaflet::clearMarkers()
  })

  # # add legend on top of leaflet object
  # observe({
  #   req(map_active_df())
  #   req(app_data$map_drawn == 1)
  #
  #   if ("sf" %in% class(map_active_df()) &
  #     is.atomic(map_active_df()[[map_var()]]) &
  #     nrow(map_active_df()) > 0) {
  #
  #     # Catch GeoPackages with non-spatial tables that GeoPandas has added empty
  #     # GeometryCollection column to.
  #     if (any(is.na(sf::st_crs(map_active_df())))) {
  #       return()
  #     }
  #
  #     # make map active layer epsg 4326
  #     # make this an if statement
  #     map_df <- try(
  #       map_active_df() %>%
  #         sf::st_transform(4326)
  #     )
  #
  #     if ("try-error" %in% class(map_df)) {
  #       return()
  #     }
  #
  #     bbox <- sf::st_bbox(map_df) %>%
  #       as.vector()
  #
  #     if (class(map_df[[map_var()]]) != "numeric" &
  #       class(map_df[[map_var()]]) != "integer") {
  #       pal <- leaflet::colorFactor(input$map_colour, map_df[[map_var()]])
  #     } else {
  #       pal <- leaflet::colorNumeric(input$map_colour, map_df[[map_var()]])
  #     }
  #
  #     if (input$legend == TRUE) {
  #       leaflet::leafletProxy("web_map") %>%
  #         leaflet::clearControls() %>%
  #         leaflet::addLegend(
  #           pal = pal,
  #           values = map_df[[map_var()]],
  #           position = "topright",
  #           title = input$map_legend_title
  #         )
  #     } else {
  #       leaflet::leafletProxy("webMap") %>%
  #         leaflet::clearControls()
  #     }
  #   }
  # })

  observeEvent(input$legend, {
    req(map_active_df())
    req(app_data$map_drawn == 1)

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

    if (input$legend == FALSE) {
      leaflet::leafletProxy("web_map") %>%
        leaflet::clearControls()
    }
  })

  # Charts ------------------------------------------------------------------

  resize_waiter <- waiter::Waiter$new(
    html = resize_screen,
    color = "rgba(89,49,150,.6)"
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

  # END ---------------------------------------------------------------------
}
