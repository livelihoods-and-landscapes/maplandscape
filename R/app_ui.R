#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import leaflet
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    navbarPage(
      theme = bslib::bs_theme(bootswatch = "flatly"),
      "",
      collapsible = TRUE,
      id = "navbar",

      tabPanel(
        "Home",
        tags$style(HTML(
          "body {
          background-image: url('www/farm-bg-fade-2.jpg');
    
          background-position: center center;
          
          background-repeat: no-repeat;
      
          background-attachment: fixed;
          
          background-size: cover;
    
          background-color: #464646;
          -webkit-background-size: cover;
          -moz-background-size: cover;
          -o-background-size: cover;
          
          }
          "
        )),

        fluidPage(
          fluidRow(
            style = "min-height: 25%; min-height: 25vh;"
          ),

          fluidRow(
            tags$h1("map.landscape", class = "mx-auto text-center"),
            class = "justify-content-center"
          ),
          fluidRow(
            tags$h4("Quickly explore geospatial data collected using QField", class = "mx-auto text-center"),
            class = "justify-content-center"
          ),
          fluidRow(
            div(
              class = "mx-auto",
              actionButton("enter", htmltools::HTML("enter &#8674;"))
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
            h4("Sync Completed Forms"),

            actionButton("sync_forms", "Sync forms"),

            hr(style = "border-color: #2c3e50 !important;"),

            h4("Upload Data"),

            mod_get_layers_ui(id = "qfield_data", label = "Select .gpkg or .zip file(s)", multiple = TRUE, accept = c(".gpkg", ".zip", ".csv")),

            selectInput("active_layer", "Select active layer", choices = NULL),

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

              actionButton("table_join_button", "Join")
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

              actionButton("spatial_join_button", "Join")
            ),

            conditionalPanel(
              shinyFeedback::useShinyFeedback(),
              condition = "input.analysis == 'Filter Rows'",

              h4("Filter Rows"),

              selectInput("table_filter", label = "Select table to filter", choices = NULL),

              actionButton("filter", "Filter Options"),
            ),

            conditionalPanel(
              shinyFeedback::useShinyFeedback(),
              condition = "input.analysis == 'Add Column'",

              h4("Add New Column"),

              selectInput("table_mutate", label = "Select table to add new column", choices = NULL),

              actionButton("add_column", "Add Column Options"),
            ),
          ),

          # show data tables
          mainPanel(tabsetPanel(
            type = "tabs",

            tabPanel(
              "Data: Raw",

              br(),

              downloadButton("download_data_raw", "Download Data"),

              hr(),

              div(style = "overflow-x:scroll; overflow-y:scroll", mod_render_dt_ui(id = "data_raw"))
            ),

            tabPanel(
              "Data: Summary",

              br(),

              downloadButton("download_data_summarised", "Download Summarised Data"),

              hr(),

              div(style = "overflow-x:scroll; overflow-y:scroll", mod_render_dt_ui(id = "data_summary"))
            )
          )),
        ),
      ),


      # Map Tab -----------------------------------------------------------------

      tabPanel(
        "Map",
        tags$style(
          type = "text/css",
          "#web_map {height: calc(100vh - 60px) !important; position: fixed; top: 60px; left: 0; right: 0; bottom: 0; padding: 0; overflow: hidden;}",
          "body {
            margin: 0;
            padding: 0;
          }",
          ".leaflet-popup-content-wrapper {background-color: #ecf0f1}",
          # fill map to height of container;  https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height/36471739#36471739
          ".leaflet-map-pane { z-index: auto; }",
          ".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }"
        ),

        shinyFeedback::useShinyFeedback(),
        leafletOutput("web_map"),

        absolutePanel(
          top = "73px",
          left = "55px",
          width = "250px",
          height = "80%",
          wellPanel(
            style = "padding: 5px !important; border-color: #2c3e50 !important; background: #ecf0f1;",
            checkboxInput("map_controls", "Map controls", value = FALSE, width = NULL),
            conditionalPanel(
              condition = "input.map_controls == true",

              actionButton("create_map", "draw map"),

              selectInput("map_active_layer", "Select active layer", choices = NULL),

              mod_single_input_ui(id = "map_var", label = "Select variable"),

              selectInput("map_colour", "Fill colour palette", choices = colour_mappings),

              sliderInput("opacity", "Opacity:",
                min = 0, max = 1,
                value = 0.8, step = 0.1
              ),

              numericInput("map_line_width", "Line width", 0.5, min = 0, max = 2),

              selectInput("map_line_colour", "Select line colour", choices = line_colours),

              helpText("Check box to display legend."),

              checkboxInput("legend", label = "Legend", value = FALSE),

              textInput("map_legend_title", "Legend title:", value = ""),

              mod_multiple_input_ui(id = "label_vars", label = "Popup labels")
            )
          )
        )
      ),


      # Charts Tab --------------------------------------------------------------

      tabPanel(
        "Charts",

        waiter::use_waiter(),
        sidebarLayout(
          # Sidebar panel for inputs ----
          sidebarPanel(
            h4("Charts"),

            selectInput("chart_active_layer", "Select active layer", choices = NULL),

            actionButton("create_chart", "draw chart"),

            selectInput(
              "plotType", "Chart type",
              c("histogram", "scatter", "bar plot")
            ),

            conditionalPanel(
              condition = "input.plotType == 'histogram'",

              mod_single_input_ui(id = "hist_x_axis_var", label = "X-axis variable"),

              numericInput("binwidth", "Histogram bin width", 100)
            ),

            conditionalPanel(
              condition = "input.plotType == 'scatter'",

              mod_single_input_ui(id = "scatter_x_axis_var", label = "X-axis variable"),

              mod_single_input_ui(id = "scatter_y_axis_var", label = "Y-axis variable"),

              numericInput("scatter_point_size", "Point size",
                min = 1, max = 20,
                value = 3, step = 0.5
              )
            ),

            conditionalPanel(
              condition = "input.plotType == 'bar plot'",

              mod_single_input_ui(id = "col_grouping_var", label = "Grouping variable"),

              mod_single_input_ui(id = "col_summarising_var", label = "Summary variable"),

              radioButtons(
                "bar_plot_type", "Bar plot type:",
                c(
                  "count" = "count_records",
                  "sum" = "sum_values",
                  "mean" = "mean"
                )
              )
            ),

            sliderInput("chart_height",
              "Chart Height:",
              min = 100,
              max = 1500,
              value = 400,
              step = 100
            ),

            textInput("x_axis_label", "X-axis label", ""),

            textInput("y_axis_label", "Y-axis label", ""),

            numericInput("lab_font", "Axis label - text size",
              min = 10, max = 36,
              value = 14, step = 1
            ),

            numericInput("axis_font", "Axis value - text size",
              min = 6, max = 35,
              value = 10, step = 1
            ),
          ),

          mainPanel(
            plotOutput("chart", height = "auto")
          )
        )
      ),


      # Admin Tab ---------------------------------------------------------------

      tabPanel(
        "Admin",
      ),

      # Docs Tab ----------------------------------------------------------------

      tabPanel(
        "Tonga Map",
        shinyFeedback::useShinyFeedback(),
        sidebarLayout(
          sidebarPanel(

            fileInput(
              "tonga_data",
              "Select .gpkg or .zip file(s)",
              accept = ".gpkg"
            ),

            shiny::tags$br(),

            selectInput(
              "tonga_layers",
              label = "Select Tonga data",
              choices = c(
                "Tonga crop survey (raw)",
                "Tonga crop survey (block)",
                "Tonga crop survey (village)",
                "Tonga crop survey (district)"
              )
            ),

            conditionalPanel(
              condition = "input.tonga_data_view == 't_map'",

              shiny::tags$br(),

              selectInput(
                "tonga_map_layer",
                label = "Select layer",
                choices = NULL
              ),

              selectInput(
                "tonga_map_colour",
                "Fill colour palette",
                choices = colour_mappings
              ),

              mod_multiple_input_ui(
                id = "tonga_label_vars",
                label = "Popup labels"
              )
            ),

            conditionalPanel(
              condition = "input.tonga_data_view == 't_table'",

              shiny::tags$br(),

              mod_multiple_input_ui(
                id = "tonga_table_layers",
                label = "Select layer(s)"
              )
            ),

            conditionalPanel(
              condition = "input.tonga_data_view == 't_chart'",

              shiny::tags$br(),

              selectInput(
                "tonga_chart_layer",
                label = "Select layer",
                choices = NULL
              ),

              sliderInput(
                "tonga_chart_height",
                "Chart Height:",
                min = 100,
                max = 1500,
                value = 400,
                step = 100
              ),

              textInput(
                "tonga_x_axis_label",
                "X-axis label",
                ""
              ),

              textInput(
                "tonga_y_axis_label",
                "Y-axis label",
                ""
              ),

              numericInput(
                "tonga_lab_font",
                "Axis label - text size",
                min = 10,
                max = 36,
                value = 14,
                step = 1
              ),

              numericInput(
                "tonga_axis_font",
                "Axis value - text size",
                min = 6,
                max = 35,
                value = 14,
                step = 1
              ),
            ),
            width = 3
          ),

          mainPanel(tabsetPanel(
            type = "tabs",
            id = "tonga_data_view",

            tabPanel(
              "Map",
              value = "t_map",

              tags$style(
                type = "text/css",
                "#tonga_leafmap {height: calc(100vh - 135px) !important;}",
                "body {
                          margin: 0;
                          padding: 0;
                       }",
                ".leaflet-popup-content-wrapper {background-color: #ecf0f1}",
                # fill map to height of container;  https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height/36471739#36471739
                ".leaflet-map-pane { z-index: auto; }",
                ".shiny-notification {
                         position:fixed;
                         top: calc(50%);
                         left: calc(50%);
                       }"
              ),
              leafletOutput("tonga_leafmap"),
            ),

            tabPanel(
              "Table",
              value = "t_table",

              shiny::tags$br(),

              downloadButton(
                "download_tonga_data",
                "Download Data"
              ),

              hr(),

              div(
                style = "overflow-x:scroll; overflow-y:scroll",
                mod_render_dt_ui(
                  id = "tonga_data_dt"
                )
              )
            ),

            tabPanel(
              "Chart",
              value = "t_chart",
              plotOutput("tonga_chart", height = "auto")
            )
          ),
          width = 9
          )
        )
      ),

      # Docs tab ----------------------------------------------------------------

      tabPanel(
        "Documentation",
        fixedPage(
          tabsetPanel(
            tabPanel(
              "About",
              tags$div(
                style = "padding-top: 10px;
                                padding-right: 10px;
                                padding-bottom: 10px;
                                padding-left: 10px;",

                tags$h2("About", style = "text-align:left;"),

                tags$br(),

                tags$div(HTML("<em>map.landscape</em> is developed as part of the ACIAR funded <a href='https://livelihoods-and-landscapes.com' target='_blank'>livelihoods and landscapes</a> project. 
                       This project is funded by the Australian Centre for International Agricultural Research (ACIAR) and is a collaboration between stakeholders in Fiji, Tonga, Australia, and New Zealand.")),

                tags$br(),

                tags$div(HTML("<em>map.landscape</em> forms part of workflow developed to map agricultural landscapes and record information about farm management and condition. 
                       This workflow encompasses collecting geospatial data in the field using QField mobile GIS, Express server-side apps to support collating data collected on different mobile devices, and <em>map.landscape</em> for data processing and visualisation. 
                       <em>map.landscape</em> also includes some helper functions for characterising farm diversity.")),

                tags$br(),

                tags$img(
                  src = "www/workflow.png",
                  width = "70%",
                  height = "auto",
                  style = "display: block;
                           margin-left: auto; 
                           margin-right: auto;
                           padding: 5px;
                           border: thin silver solid;"
                ),

                tags$br(),

                tags$div(HTML("Read more about this workflow <a href='https://livelihoods-and-landscapes.com' target='_blank'>here</a>.")),

                hr(style = "border-color: #2c3e50 !important;")
              )
            ),
            tabPanel(
              "Docs",
              tags$div(
                style = "padding-top: 10px;
                                padding-right: 10px;
                                padding-bottom: 10px;
                                padding-left: 10px;",
              ),
              tags$div(HTML("Documentation and vignettes for <em>map.landscape</em> can be found <a href='https://livelihoods-and-landscapes.com/maplandscape' target='_blank'>here</a>"))
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "maplandscape"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
