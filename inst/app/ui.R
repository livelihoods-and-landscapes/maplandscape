

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

    fluidPage(
      # Loading screen
      # div(
      #   id = "loading-screen",
      #   fluidRow(
      #     style = "min-height: 25%; min-height: 25vh;"
      #   ),
      #   fluidRow(
      #     style = "min-height: 25%; min-height: 25vh;",
      #     h2("Loading app...",
      #       class = "mx-auto text-center"
      #     )
      #   )
      # ),
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

  # tabPanel(
  #   "Data",
  #   sidebarLayout(
  #     sidebarPanel(),
  #     mainPanel()
  #   )
  # ),

  # Table Tab ---------------------------------------------------------------

  tabPanel(
    "Table",
    shinyFeedback::useShinyFeedback(),
    waiter::use_waiter(),
    sidebarLayout(
      sidebarPanel(
        class = "sidePanelStyle",
        h4("Active layer"),
        selectInput(
          "active_layer",
          "Select active layer",
          choices = NULL
        ),
        hr(style = "border-color: #5a5a5a !important;"),
        h4("Upload data"),
        mod_get_layers_UI(
          id = "user_data",
          label = "Select .gpkg or .zip file(s)",
          multiple = TRUE,
          accept = c(".gpkg", ".zip", ".csv")
        ),

        # QFieldCloud login
        hr(style = "border-color: #343a40 !important;"),
        h4("QFieldCloud data"),
        textInput("qfieldcloud_url",
          "QFieldCloud app URL",
          value = "",
          placeholder = "tip: omit https:// and trailing /"
        ),
        textInput("qfieldcloud_username",
          "QFieldCloud email",
          value = "",
          placeholder = ""
        ),
        passwordInput("qfieldcloud_password",
          "QFieldCloud password",
          value = "",
          placeholder = ""
        ),
        actionButton(
          "qfieldcloud_login",
          "Login to QFieldCloud",
          class = "btn-primary m-2"
        ),
        uiOutput("qfieldcloud_login_status"),
        actionButton(
          "list_qfieldcloud_projects",
          "Get QFieldCloud projects",
          class = "btn-primary m-2"
        ),
        selectInput(
          "qfieldcloud_projects",
          "Select QFieldCloud project",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        selectInput(
          "qfieldcloud_gpkg",
          "Select QFieldCloud dataset",
          choices = NULL,
          selected = NULL,
          multiple = FALSE
        ),
        actionButton(
          "get_qfieldcloud_gpkg",
          "Download QFieldCloud file",
          class = "btn-primary m-2"
        ),

        # table analysis
        hr(style = "border-color: #5a5a5a !important;"),
        h4("Table analysis"),
        selectInput(
          "analysis", "Select analysis",
          c("Summary tables", "Combine tables", "Combine spatial layers", "Filter rows", "Add column")
        ),
        conditionalPanel(
          condition = "input.analysis == 'Summary tables'",
          h4("Summary tables"),
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
          condition = "input.analysis == 'Combine tables'",
          h4("Combine tables"),
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
            "Layer name",
            value = "",
            placeholder = "enter layer name for output"
          ),
          actionButton(
            "table_join_button",
            "Join",
            class = "btn-primary m-2"
          )
        ),
        conditionalPanel(
          condition = "input.analysis == 'Combine spatial layers'",
          h4("Combine spatial layers"),
          selectInput(
            "spatial_table_left",
            label = "Select left layer in join",
            choices = NULL
          ),
          selectInput(
            "spatial_table_right",
            label = "Select right layer in join",
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
            "Layer name",
            value = "",
            placeholder = "enter layer name for output"
          ),
          actionButton(
            "spatial_join_button",
            "Join",
            class = "btn-primary m-2"
          )
        ),
        conditionalPanel(
          condition = "input.analysis == 'Filter rows'",
          h4("Filter rows"),
          selectInput(
            "table_filter",
            label = "Select layer to filter",
            choices = NULL
          ),
          actionButton(
            "filter",
            "Filter options",
            class = "btn-primary m-2"
          ),
        ),
        conditionalPanel(
          condition = "input.analysis == 'Add column'",
          h4("Add new column"),
          selectInput(
            "table_mutate",
            label = "Select layer to add new column",
            choices = NULL
          ),
          actionButton(
            "add_column",
            "Add column options",
            class = "btn-primary m-2"
          ),
        ),
      ),

      # show data tables
      mainPanel(
        class = "mainPanelStyle",
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Data: Raw",
            downloadButton(
              "download_data_raw",
              "Download Data",
              class = "btn-primary m-2"
            ),
            hr(),
            mod_render_dt_UI(id = "data_raw")
          ),
          tabPanel(
            "Data: Summary",
            downloadButton(
              "download_data_summarised",
              "Download Summarised Data",
              class = "btn-primary m-2"
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
        h4("Map"),
        actionButton(
          "create_map",
          "Draw map",
          class = "btn-primary m-2"
        ),
        selectInput(
          "map_active_layer",
          "Active layer",
          choices = NULL
        ),
        mod_single_input_UI(
          id = "map_var",
          label = "Attribute"
        ),
        selectInput(
          "map_colour",
          "Fill colour palette:",
          choices = colour_mappings
        ),
        sliderInput(
          "opacity",
          "Opacity",
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
          "Line colour",
          choices = line_colours
        ),
        helpText("Check box to display legend:"),
        checkboxInput(
          "legend",
          label = "Legend",
          value = FALSE
        ),
        textInput(
          "map_legend_title",
          "Legend title",
          value = ""
        ),
        mod_multiple_input_UI(
          id = "label_vars",
          label = "Popup labels"
        )
      ),
      mainPanel(
        leaflet::leafletOutput("web_map")
      )
    )
  ),

  # Charts Tab --------------------------------------------------------------

  tabPanel(
    "Charts",
    sidebarLayout(
      sidebarPanel(
        class = "sidePanelStyle",
        h4("Charts"),
        selectInput(
          "chart_active_layer",
          "Active layer",
          choices = NULL
        ),
        actionButton(
          "create_chart",
          "Draw chart",
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
            label = "Grouping attribute"
          ),
          mod_single_input_UI(
            id = "col_summarising_var",
            label = "Summary attribute"
          ),
          radioButtons(
            "bar_plot_type",
            "Bar plot type",
            c(
              "count" = "count_records",
              "sum" = "sum_values",
              "mean" = "mean"
            )
          )
        ),
        sliderInput(
          "chart_height",
          "Chart Height",
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
