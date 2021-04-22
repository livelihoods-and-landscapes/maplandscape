#' render_dt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_render_dt_ui <- function(id) {
  tagList(
    DT::dataTableOutput(NS(id, "data_table"))
  )
}

#' render_dt Server Function
#'
#' @noRd
mod_render_dt_server <- function(id, dt, editable) {
  moduleServer(id, function(input, output, session) {
    output$data_table <- DT::renderDataTable({
      req(dt())
      DT::datatable(
        data = dt(),
        editable = editable
      )
    })
  })
}

## To be copied in the UI
# mod_render_dt_ui("render_dt_ui_1")

## To be copied in the server
# callModule(mod_render_dt_server, "render_dt_ui_1")
