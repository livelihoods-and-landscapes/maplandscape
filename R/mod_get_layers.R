#' get_layers UI
#'
#' @description Generate UI for file upload using shiny's \code{fileInput}.
#'
#' @param id inputID for file upload UI component.
#' @param label label for file upload widget on the UI.
#' @param multiple logical, whether to accept multiple file uploads.
#' @param accept file types to accept - vector of strings indicating extensions or MIME type
#'
#' @import shiny
#' @export
#'
mod_get_layers_UI <- function(id, label, multiple, accept) {
  tagList(
    # Input: Select a file ----
    fileInput(NS(id, "get_layers"),
      label,
      multiple = multiple,
      accept = accept
    ),
  )
}

#' get_layers Server
#'
#' @description Process files (GeoPackages) uploaded by the user. For each file
#'   uploaded, call \code{list_layers()} to list all layers in the GeoPacakge
#'   and the temporary location of the GeoPackage.
#' @param id inputID to correspond to file upload UI component.
#'
#' @return \code{field_layers} tibble with three columns: layers - layer name;
#'   layer_disp_name - clean and informative layer name for select
#'   input; file_path - temporary file path to data; and file_type - file type.
#'
#' @import shiny
#'
#' @export

mod_get_layers_Server <- function(id) {
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
