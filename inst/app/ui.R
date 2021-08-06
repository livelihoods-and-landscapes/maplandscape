library(shiny)

navbarPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  "",
  collapsible = TRUE,
  id = "navbar",

  tabPanel(
    "Home",
    includeCSS(file.path('www', 'style.css')),

    fluidPage(
      fluidRow(
        style = "min-height: 25%; min-height: 25vh;"
      ),

      fluidRow(
        tags$h1(
          "map.landscape",
          class = "mx-auto text-center"
          ),
        class = "justify-content-center"
      ),
      fluidRow(
        tags$h4(
          "Quickly explore geospatial data collected using QField",
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
    waiter::use_waiter(),

    sidebarLayout(
      # Sidebar panel for inputs ----
      sidebarPanel(
        id = "tPanel",
        style = "overflow-y:scroll; max-height: 90vh; position:relative;",

        h4("Active Layer"),

        selectInput("active_layer", "Select active layer", choices = NULL),

        hr(style = "border-color: #2c3e50 !important;"),

        h4("Sync Completed Forms"),

        actionButton("sync_forms", "Sync forms"),

        hr(style = "border-color: #2c3e50 !important;"),

        h4("Upload Data"),

        mod_get_layers_ui(id = "qfield_data", label = "Select .gpkg or .zip file(s)", multiple = TRUE, accept = c(".gpkg", ".zip", ".csv")),

        hr(style = "border-color: #2c3e50 !important;"),

        h4("Google Cloud Data"),

        # Google Cloud login button - show when there is not a valid token
        uiOutput("login_warning"),

        uiOutput("login_button"),

        # Google Cloud Storage project
        textInput("gcs_project_id",
                  "Google Cloud Storage project ID",
                  value = "",
                  placeholder = ""
        ),

        # get list of GeoPackages in Google Cloud Storage bucket
        actionButton("list_google_files", "Get GCS buckets", class = "btn-primary m-2"),

        # name of Google Cloud Storage bucket to get GeoPackages from
        selectInput(
          "gcs_bucket_name",
          "GCS bucket name",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),

        selectInput(
          "gcs_bucket_objects",
          "Select object from GCS",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),

        # get list of GeoPackages in Google Cloud Storage bucket
        actionButton("get_objects", "Download GCS object", class = "btn-primary m-2"),

        hr(style = "border-color: #2c3e50 !important;"),

        h4("Table Analysis"),

        selectInput(
          "analysis", "Select analysis",
          c("Summary Tables", "Combine Tables", "Combine Spatial Layers", "Filter Rows", "Add Column")
        ),

        conditionalPanel(
          condition = "input.analysis == 'Summary Tables'",

          h4("Summary Tables"),

          mod_multiple_input_ui(id = "grouping_var", label = "Grouping variable(s)"),

          mod_multiple_input_ui(id = "summarising_var", label = "Summarising variable(s)")
        ),

        conditionalPanel(
          condition = "input.analysis == 'Combine Tables'",

          h4("Combine Tables"),

          selectInput("table_left", label = "Select left table in join", choices = NULL),

          selectInput("table_right", label = "Select right table in join", choices = NULL),

          mod_multiple_input_ui(id = "joining_p_key_left", label = "Select primary key(s) - left table"),

          mod_multiple_input_ui(id = "joining_f_key_right", label = "Select foreign key(s) - right table"),

          radioButtons("key_join_type", "Join Type:",
                       c(
                         "column - inner" = "col_inner",
                         "column - left" = "col_left"
                       ),
                       selected = NULL
          ),

          textInput("join_tbl_name", "Table name", value = "", placeholder = "enter table name for output"),

          actionButton("table_join_button", "Join", class = "btn-primary m-2")
        ),

        conditionalPanel(
          shinyFeedback::useShinyFeedback(),
          condition = "input.analysis == 'Combine Spatial Layers'",

          h4("Combine Spatial Layers"),

          selectInput("spatial_table_left", label = "Select left table in join", choices = NULL),

          selectInput("spatial_table_right", label = "Select right table in join", choices = NULL),

          radioButtons("spatial_join_type", "Join Type:",
                       c(
                         "spatial - inner" = "spatial_inner",
                         "spatial - left" = "spatial_left"
                       ),
                       selected = NULL
          ),

          textInput("spjoin_tbl_name", "Table name", value = "", placeholder = "enter table name for output"),

          actionButton("spatial_join_button", "Join", class = "btn-primary m-2")
        ),

        conditionalPanel(
          shinyFeedback::useShinyFeedback(),
          condition = "input.analysis == 'Filter Rows'",

          h4("Filter Rows"),

          selectInput("table_filter", label = "Select table to filter", choices = NULL),

          actionButton("filter", "Filter Options", class = "btn-primary m-2"),
        ),

        conditionalPanel(
          shinyFeedback::useShinyFeedback(),
          condition = "input.analysis == 'Add Column'",

          h4("Add New Column"),

          selectInput("table_mutate", label = "Select table to add new column", choices = NULL),

          actionButton("add_column", "Add Column Options", class = "btn-primary m-2"),
        ),
      ),

      # show data tables
      mainPanel(tabsetPanel(
        type = "tabs",

        tabPanel(
          "Data: Raw",

          br(),

          downloadButton("download_data_raw", "Download Data", class = "btn-primary m-2"),

          hr(),

          div(style = "overflow-x:scroll; overflow-y:scroll", mod_render_dt_ui(id = "data_raw"))
        ),

        tabPanel(
          "Data: Summary",

          br(),

          downloadButton("download_data_summarised", "Download Summarised Data", class = "btn-primary m-2"),

          hr(),

          div(style = "overflow-x:scroll; overflow-y:scroll", mod_render_dt_ui(id = "data_summary"))
        )
      )),
    ),
  )
)
