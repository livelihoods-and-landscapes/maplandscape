

navbarPage(
  theme = bslib::bs_theme(bootswatch = "pulse", version = 4),
  "",
  collapsible = TRUE,
  id = "navbar",


  # Landing Page ------------------------------------------------------------

  tabPanel(
    "Home",
    shinyjs::useShinyjs(),
    includeCSS(file.path("www", "style.css")),
    shinyFeedback::useShinyFeedback(),
    waiter::use_waiter(),
    fluidPage(
      # Loading screen
      div(
        id = "loading-screen",
        fluidRow(
          style = "min-height: 25%; min-height: 25vh;"
        ),
        fluidRow(
          style = "min-height: 25%; min-height: 25vh;",
          h2("Loading app...",
            class = "mx-auto text-center"
          )
        )
      ),
      fluidRow(
        style = "min-height: 25%; min-height: 25vh;"
      ),
      fluidRow(
        tags$h1(
          "maplandscape",
          class = "mx-auto text-center"
        ),
        class = "justify-content-center"
      ),
      fluidRow(
        tags$h4(
          "Explore QFieldCloud data",
          class = "mx-auto text-center"
        ),
        class = "justify-content-center"
      ),
      fluidRow(
        div(
          class = "mx-auto",
          actionButton(
            "enter",
            htmltools::HTML("enter &#8674;")
          )
        ),
        class = "justify-content-center"
      )
    )
  ),

  # Data Tab ----------------------------------------------------------------

  tabPanel(
    "Data",
    fluidPage(
      class="scrollPage",
      fluidRow(
      column(
        4,

        # QFieldCloud login
        h4("QFieldCloud data"),

        # QFieldCloud app URL
        textInput("qfieldcloud_url",
          "QFieldCloud app URL:",
          value = "",
          placeholder = "tip: omit https:// and trailing /"
        ),

        # QFieldCloud login
        textInput("qfieldcloud_username",
          "QFieldCloud email:",
          value = "",
          placeholder = ""
        ),

        # QFieldCloud password
        passwordInput("qfieldcloud_password",
          "QFieldCloud password:",
          value = "",
          placeholder = ""
        ),

        # Try logging into QFieldCloud
        actionButton(
          "login",
          "Login to QFieldCloud",
          class = "btn mb-4"
        ),


        # QFieldCloud login status
        uiOutput("login_status"),

        # Select QField Cloud project
        selectInput(
          "qfieldcloud_projects",
          "Select QFieldCloud project:",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),

        # Select file from QFieldCloud
        selectInput(
          "qfieldcloud_gpkg",
          "Select QFieldCloud dataset:",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),

        # Download GeoPackage from QFieldCloud
        actionButton(
          "get_qfieldcloud_gpkg",
          "Download QFieldCloud file",
          class = "btn mb-4"
        ),

      ),
      column(
        8,
        fluidRow(
          column(
            6,
            # upload local data
            h4("Upload local data"),

            # file upload widget - browse local file system
            mod_get_layers_UI(
              id = "user_data",
              label = "Select GeoPackage from local storage:",
              multiple = TRUE,
              accept = c(".gpkg")
            )
          ),
          column(
            6,
            # sync GeoPackages in app
            h4("Sync GeoPackages"),
            p("Sync a set of GeoPackages with a template file:"),

            # bring up modal with options for syncing GeoPackages
            actionButton(
              "sync_forms",
              "Sync",
              class = "btn m-2"
            ),
          )
        ),
        fluidRow(
          column(
            12,
            class = "dt-scroll",
            h4("Layers"),
            hr(),
            mod_render_dt_UI(id = "app_layers")
          )
        )
      )
    )),
  ),

  # Table Tab ---------------------------------------------------------------

  tabPanel(
    "Table",

    sidebarLayout(
      sidebarPanel(
        class = "sidePanelStyle",

        # Select active layer
        h4("Active layer"),
        selectInput(
          "active_layer",
          "Select active layer:",
          choices = NULL
        ),
        hr(),
        h4("Filter rows"),
        selectInput(
          "table_filter",
          label = "Select layer to filter:",
          choices = NULL
        ),
        div(
          class = "d-flex justify-content-center",
          actionButton(
            "filter",
            "Filter rows options",
            class = "btn btn-block m-2"
          ),
        ),
        hr(),
        h4("Add new column"),
        selectInput(
          "table_mutate",
          label = "Select layer to add new column:",
          choices = NULL
        ),
        div(
          class = "d-flex justify-content-center",
          actionButton(
            "add_column",
            "Add column options",
            class = "btn btn-block m-2"
          ),
        ),
        hr(),
        # Summary tables
        h4("Summary tables"),
        mod_multiple_input_UI(
          id = "grouping_var",
          label = "Grouping variable(s):"
        ),
        mod_multiple_input_UI(
          id = "summarising_var",
          label = "Summarising variable(s):"
        ),
        hr(),
        h4("Combine layers"),
        selectInput(
          "table_left",
          label = "Select left layer in join:",
          choices = NULL
        ),
        selectInput(
          "table_right",
          label =
            "Select right layer in join:",
          choices = NULL
        ),
        mod_multiple_input_UI(
          id = "joining_p_key_left",
          label = "Select primary key(s) - left layer:"
        ),
        mod_multiple_input_UI(
          id = "joining_f_key_right",
          label = "Select foreign key(s) - right layer:"
        ),
        radioButtons(
          "key_join_type",
          "Join Type:",
          c(
            "column - inner" = "col_inner",
            "column - left" = "col_left"
          ),
          selected = NULL
        ),
        textInput(
          "join_tbl_name",
          "Layer name:",
          value = "",
          placeholder = "enter layer name for output"
        ),
        div(
          class = "d-flex justify-content-center",
          actionButton(
            "table_join_button",
            "Apply join",
            class = "btn btn-block m-2"
          ),
        ),
        hr(),
        h4("Combine spatial layers"),
        selectInput(
          "spatial_table_left",
          label = "Select left layer in join:",
          choices = NULL
        ),
        selectInput(
          "spatial_table_right",
          label = "Select right layer in join:",
          choices = NULL
        ),
        textInput(
          "spjoin_tbl_name",
          "Layer name:",
          value = "",
          placeholder = "enter layer name for output"
        ),
        div(
          class = "d-flex justify-content-center",
          actionButton(
            "spatial_join_button",
            "Apply spatial join",
            class = "btn btn-block m-2"
          ),
        ),
      ),

      # show data tables
      mainPanel(
        class = "mainPanelStyle",
        tabsetPanel(
          id = "data_tables",
          type = "tabs",
          tabPanel(
            "Data: Raw",
            downloadButton(
              "download_data_raw",
              "Download Data",
              class = "btn m-2"
            ),
            hr(),
            mod_render_dt_UI(id = "data_raw")
          ),
          tabPanel(
            "Data: Summary",
            downloadButton(
              "download_data_summarised",
              "Download Summarised Data",
              class = "btn m-2"
            ),
            hr(),
            mod_render_dt_UI(id = "data_summary")
          )
        )
      ),
    ),
  ),

  # Map Tab -----------------------------------------------------------------

  tabPanel(
    "Map",
    sidebarLayout(
      sidebarPanel(
        class = "sidePanelStyle",
        h4("Active layer"),
        selectInput(
          "map_active_layer",
          "Select active layer",
          choices = NULL
        ),
        hr(),
        mod_single_input_UI(
          id = "map_var",
          label = "Select column:"
        ),
        div(
          class = "d-flex justify-content-center",
          actionButton(
            "create_map",
            "Draw map",
            class = "btn btn-block m-2"
          ),
        ),
        hr(),
        p("Recenter map data if it crosses the antimeridian."),
        div(
          class = "d-flex justify-content-center",
          actionButton(
            "recenter_map",
            "Recenter map",
            class = "btn btn-block m-2"
          ),
        ),
        hr(),
        selectInput(
          "map_colour",
          "Fill colour palette:",
          choices = colour_mappings
        ),
        div(
          class = "mx-auto m-2",
          plotOutput(
            "colour_ramp",
            width = "100%",
            height = "30px"
          )
        ),
        helpText("Check box to display legend:"),
        checkboxInput(
          "legend",
          label = "Legend",
          value = FALSE
        ),
        mod_multiple_input_UI(
          id = "label_vars",
          label = "Popup labels:"
        ),
        div(
          class = "d-flex justify-content-center",
          actionButton(
            "add_popups",
            "Add popups to map",
            class = "btn btn-block m-2"
          ),
        )
      ),
      mainPanel(
        leafgl::leafglOutput("web_map")
      )
    )
  ),

  # Charts Tab --------------------------------------------------------------

  tabPanel(
    "Charts",
    sidebarLayout(
      sidebarPanel(
        class = "sidePanelStyle",
        h4("Active layer"),
        selectInput(
          "chart_active_layer",
          "Select active layer:",
          choices = NULL
        ),
        hr(),
        div(
          class = "d-flex justify-content-center",
          actionButton(
            "create_chart",
            "Draw chart",
            class = "btn btn-block m-2"
          ),
        ),
        sliderInput(
          "chart_height",
          "Chart Height:",
          min = 100,
          max = 1500,
          value = 400,
          step = 100
        ),
        hr(),
        selectInput(
          "plotType",
          "Select chart type:",
          c("histogram", "scatter", "bar plot")
        ),
        conditionalPanel(
          condition = "input.plotType == 'histogram'",
          mod_single_input_UI(
            id = "hist_x_axis_var",
            label = "X-axis variable:"
          ),
          numericInput(
            "binwidth",
            "Histogram bin width:",
            100
          )
        ),
        conditionalPanel(
          condition = "input.plotType == 'scatter'",
          mod_single_input_UI(
            id = "scatter_x_axis_var",
            label = "X-axis column:"
          ),
          mod_single_input_UI(
            id = "scatter_y_axis_var",
            label = "Y-axis column:"
          ),
          numericInput(
            "scatter_point_size",
            "Point size:",
            min = 1,
            max = 20,
            value = 3,
            step = 0.5
          )
        ),
        conditionalPanel(
          condition = "input.plotType == 'bar plot'",
          mod_single_input_UI(
            id = "col_grouping_var",
            label = "Grouping column:"
          ),
          mod_single_input_UI(
            id = "col_summarising_var",
            label = "Summary column"
          ),
          radioButtons(
            "bar_plot_type",
            "Bar plot type:",
            c(
              "count" = "count_records",
              "sum" = "sum_values",
              "mean" = "mean"
            )
          )
        ),
        textInput(
          "x_axis_label",
          "X-axis label:",
          ""
        ),
        textInput(
          "y_axis_label",
          "Y-axis label:",
          ""
        ),
        numericInput(
          "lab_font",
          "Axis label - text size:",
          min = 10,
          max = 36,
          value = 14,
          step = 1
        ),
        numericInput(
          "axis_font",
          "Axis value - text size:",
          min = 6,
          max = 35,
          value = 10,
          step = 1
        ),
      ),
      mainPanel(
        plotOutput(
          "chart",
          height = "auto"
        )
      )
    )
  )
)
