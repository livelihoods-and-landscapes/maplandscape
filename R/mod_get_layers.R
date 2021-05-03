#' get_layers UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_get_layers_ui <- function(id, label, multiple, accept) {
  tagList(
    # Input: Select a file ----
    fileInput(NS(id, "get_layers"), label,
      multiple = multiple,
      accept = accept
    ),
  )
}

#' get_layers Server Function
#'
#' @noRd
mod_get_layers_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    field_layers <- reactive({
      req(input$get_layers$datapath)

      n_file <- input$get_layers$datapath
      n_name <- input$get_layers$name

      f_lyrs <- tryCatch(
        error = function(cnd) NULL,
        purrr::map2(n_file, n_name, list_layers) %>%
          dplyr::bind_rows()
      )
    })
    return(field_layers)
  })
}

## To be copied in the UI
# mod_get_layers_ui("get_layers_ui_1")

## To be copied in the server
# callModule(mod_get_layers_server, "get_layers_ui_1")
