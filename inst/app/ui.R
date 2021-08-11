library(shiny)
library(maplandscape)

navbarPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  "",
  collapsible = TRUE,
  id = "navbar",

  tabPanel(
    "Home",
    includeCSS(file.path("www", "style.css")),

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
          "Quickly explore geospatial data",
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
    shinyFeedback::useShinyFeedback(),
    sidebarLayout(
      # Sidebar panel for inputs ----
      sidebarPanel(
        id = "dataSidePanel",

        h4("Active Layer"),

        selectInput(
          "active_layer",
          "Select active layer",
          choices = NULL
        ),

        hr(style = "border-color: #5a5a5a !important;"),

        h4("Sync Completed Forms"),

        actionButton(
          "sync_forms",
          "Sync forms"
        ),

        hr(style = "border-color: #5a5a5a !important;"),

        h4("Upload Data"),

        mod_get_layers_UI(
          id = "user_data",
          label = "Select .gpkg or .zip file(s)",
          multiple = TRUE,
          accept = c(".gpkg", ".zip", ".csv")
        ),

        hr(style = "border-color: #343a40 !important;"),

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
        actionButton(
          "list_google_files",
          "Get GCS buckets",
          class = "btn-primary m-2"
          ),

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
        actionButton(
          "get_objects",
          "Download GCS object",
          class = "btn-primary m-2"
        ),

        hr(style = "border-color: #5a5a5a !important;"),

        h4("Table Analysis"),

        selectInput(
          "analysis", "Select analysis",
          c("Summary Tables", "Combine Tables", "Combine Spatial Layers", "Filter Rows", "Add Column")
        ),

        conditionalPanel(
          condition = "input.analysis == 'Summary Tables'",

          h4("Summary Tables"),

          mod_multiple_input_UI(
            id = "grouping_var",
            label = "Grouping variable(s)"
          ),

          mod_multiple_input_UI(
            id = "summarising_var",
            label = "Summarising variable(s)"
          )
        ),

        conditionalPanel(
          condition = "input.analysis == 'Combine Tables'",

          h4("Combine Tables"),

          selectInput(
            "table_left",
            label = "Select left table in join",
            choices = NULL
          ),

          selectInput(
            "table_right",
            label =
              "Select right table in join",
            choices = NULL
          ),

          mod_multiple_input_UI(
            id = "joining_p_key_left",
            label = "Select primary key(s) - left table"
          ),

          mod_multiple_input_UI(
            id = "joining_f_key_right",
            label = "Select foreign key(s) - right table"
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
            "Table name",
            value = "",
            placeholder = "enter table name for output"
          ),

          actionButton(
            "table_join_button",
            "Join",
            class = "btn-primary m-2"
          )
        ),

        conditionalPanel(
          condition = "input.analysis == 'Combine Spatial Layers'",

          h4("Combine Spatial Layers"),

          selectInput(
            "spatial_table_left",
            label = "Select left table in join",
            choices = NULL
          ),

          selectInput(
            "spatial_table_right",
            label = "Select right table in join",
            choices = NULL
          ),

          radioButtons(
            "spatial_join_type",
            "Join Type:",
            c(
              "spatial - inner" = "spatial_inner",
              "spatial - left" = "spatial_left"
            ),
            selected = NULL
          ),

          textInput(
            "spjoin_tbl_name",
            "Table name",
            value = "",
            placeholder = "enter table name for output"
          ),

          actionButton(
            "spatial_join_button",
            "Join",
            class = "btn-primary m-2"
          )
        ),

        conditionalPanel(
          condition = "input.analysis == 'Filter Rows'",

          h4("Filter Rows"),

          selectInput(
            "table_filter",
            label = "Select table to filter",
            choices = NULL
          ),

          actionButton(
            "filter",
            "Filter Options",
            class = "btn-primary m-2"
          ),
        ),

        conditionalPanel(
          condition = "input.analysis == 'Add Column'",

          h4("Add New Column"),

          selectInput(
            "table_mutate",
            label = "Select table to add new column",
            choices = NULL
          ),
          actionButton(
            "add_column",
            "Add Column Options",
            class = "btn-primary m-2"
          ),
        ),
      ),

      # show data tables
      mainPanel(tabsetPanel(
        type = "tabs",

        tabPanel(
          "Data: Raw",

          br(),

          downloadButton(
            "download_data_raw",
            "Download Data",
            class = "btn-primary m-2"
          ),

          hr(),

          div(
            style = "overflow-x:scroll; overflow-y:scroll",
            mod_render_dt_UI(id = "data_raw")
          )
        ),

        tabPanel(
          "Data: Summary",

          br(),

          downloadButton(
            "download_data_summarised",
            "Download Summarised Data",
            class = "btn-primary m-2"
          ),

          hr(),

          div(
            style = "overflow-x:scroll; overflow-y:scroll",
            mod_render_dt_UI(id = "data_summary")
          )
        )
      )),
    ),
  ),


  # Map Tab -----------------------------------------------------------------

  tabPanel(
    "Map",

    leaflet::leafletOutput("web_map"),

    absolutePanel(
      top = "55px",
      left = "0px",
      width = "250px",
      height = "100vh",
      wellPanel(
        id = "mapSidePanel",

        h4("Map controls"),

        actionButton(
          "create_map",
          "draw map",
          class = "btn-primary m-2"
        ),

        selectInput(
          "map_active_layer",
          "Select active layer",
          choices = NULL
        ),

        mod_single_input_UI(
          id = "map_var",
          label = "Select variable"
        ),

        selectInput(
          "map_colour",
          "Fill colour palette",
          choices = colour_mappings
        ),

        sliderInput(
          "opacity",
          "Opacity:",
          min = 0,
          max = 1,
          value = 0.8,
          step = 0.1
        ),

        numericInput(
          "map_line_width",
          "Line width",
          0.5,
          min = 0,
          max = 2
        ),

        selectInput(
          "map_line_colour",
          "Select line colour",
          choices = line_colours
        ),

        helpText("Check box to display legend."),

        checkboxInput(
          "legend",
          label = "Legend",
          value = FALSE
        ),

        textInput(
          "map_legend_title",
          "Legend title:",
          value = ""
        ),

        mod_multiple_input_UI(
          id = "label_vars",
          label = "Popup labels"
        )
      )
    )
  ),


  # Charts Tab --------------------------------------------------------------

  tabPanel(
    "Charts",

    sidebarLayout(
      # Sidebar panel for inputs ----
      sidebarPanel(
        id = "chartSidePanel",

        h4("Charts"),

        selectInput(
          "chart_active_layer",
          "Select active layer",
          choices = NULL
        ),

        actionButton(
          "create_chart",
          "draw chart",
          class = "btn-primary m-2"
        ),

        selectInput(
          "plotType",
          "Chart type",
          c("histogram", "scatter", "bar plot")
        ),

        conditionalPanel(
          condition = "input.plotType == 'histogram'",

          mod_single_input_UI(
            id = "hist_x_axis_var",
            label = "X-axis variable"
          ),

          numericInput(
            "binwidth",
            "Histogram bin width",
            100
          )
        ),

        conditionalPanel(
          condition = "input.plotType == 'scatter'",

          mod_single_input_UI(
            id = "scatter_x_axis_var",
            label = "X-axis variable"
          ),

          mod_single_input_UI(
            id = "scatter_y_axis_var",
            label = "Y-axis variable"
          ),

          numericInput(
            "scatter_point_size",
            "Point size",
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
            label = "Grouping variable"
          ),

          mod_single_input_UI(
            id = "col_summarising_var",
            label = "Summary variable"
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

        sliderInput(
          "chart_height",
          "Chart Height:",
          min = 100,
          max = 1500,
          value = 400,
          step = 100
        ),

        textInput(
          "x_axis_label",
          "X-axis label",
          ""
        ),

        textInput(
          "y_axis_label",
          "Y-axis label",
          ""
        ),

        numericInput(
          "lab_font",
          "Axis label - text size",
          min = 10,
          max = 36,
          value = 14,
          step = 1
        ),

        numericInput(
          "axis_font",
          "Axis value - text size",
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
  ),


  # Admin Tab ---------------------------------------------------------------

  tabPanel(
    "Admin",
    sidebarLayout(
      sidebarPanel(
        id = "adminSidePanel",

        h4("Upload local data"),

        mod_get_layers_UI(
          id = "edit_data",
          label = "Select .gpkg or .zip file(s)",
          multiple = FALSE,
          accept = c(".gpkg")
        ),

        h4("Edit Google Cloud data"),

        # Google Cloud Storage project
        textInput(
          "admin_gcs_project_id",
          "Google Cloud Storage project ID",
          value = "",
          placeholder = ""
        ),

        # get list of GeoPackages in Google Cloud Storage bucket
        actionButton(
          "admin_list_google_files",
          "Get GCS buckets",
          class = "btn-primary m-2"
        ),

        # name of Google Cloud Storage bucket to get GeoPackages from
        selectInput(
          "admin_gcs_bucket_name",
          "GCS bucket name",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),

        selectInput(
          "admin_gcs_bucket_objects",
          "Select object from GCS",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),

        # get list of GeoPackages in Google Cloud Storage bucket
        actionButton(
          "admin_get_objects",
          "Download GCS object",
          class = "btn-primary m-2"
        ),

        hr(style = "border-color: #5a5a5a !important;"),

        h4("Editing options"),

        selectInput(
          "edit_layer",
          "Select layer to edit",
          choices = NULL
        ),

        textInput(
          inputId = "row_id",
          label = "ID column (or ID pattern)"
        ),

        actionButton(
          "save_edits",
          "save edits",
          class = "btn-primary m-2"
        ),

        actionButton(
          "delete_records",
          "delete records",
          class = "btn-primary m-2"
        ),

        hr(style = "border-color: #5a5a5a !important;"),

        # Download edits locally
        h4("Download edits"),

        downloadButton(
          "download_edits",
          "download edits",
          class = "btn-primary m-2"
        ),

        hr(style = "border-color: #5a5a5a !important;"),

        h4("Sync edits to Google Cloud Storage"),

        # URL / endpoint for sync API
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
      ),

      mainPanel(tabsetPanel(
        type = "tabs",
        id = "edit_data_view",

        tabPanel(
          "Map",
          value = "edit_map",

          tags$style(
            type = "text/css",
            "#edit_leafmap {height: calc(100vh - 135px) !important;}",
          ),
          leaflet::leafletOutput("edit_leafmap"),
        ),

        tabPanel(
          "Table",
          value = "edit_table",

          tags$br(),

          hr(),

          div(
            style = "overflow-x:scroll; overflow-y:scroll",
            mod_render_dt_UI(
              id = "edit_data_dt"
            )
          )
        )
      ))
    )
  ),

  # Docs Tab ----------------------------------------------------------------

  tabPanel(
    "Documentation",
    fixedPage(
      tabsetPanel(
        tabPanel(
          "About",
          tags$div(
            class = "docs",

            tags$h2("About", style = "text-align:left;"),

            tags$br(),

            tags$div(HTML("<em>map.landscape</em> is developed as part of the ACIAR funded <a href='https://livelihoods-and-landscapes.com' target='_blank'>livelihoods and landscapes</a> project: a collaboration between stakeholders in Fiji, Tonga, Australia, and New Zealand.")),

            tags$br()
          )
        ),
        tabPanel(
          "Docs",
          tags$div(
            class = "docs",
            tags$div(HTML("Documentation and vignettes for <em>map.landscape</em> can be found <a href='https://livelihoods-and-landscapes.com/maplandscape' target='_blank'>here</a>"))
          )
        )
      )
    )
  )
)
