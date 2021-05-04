#' single_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_single_input_ui <- function(id, label) {
  tagList(
    selectInput(NS(id, "single_input"), label, choices = NULL)
  )
}

#' single_input Server Function
#'
#' @noRd
mod_single_input_server <- function(id, s_df) {
  moduleServer(id, function(input, output, session) {

    # update select input for active layer
    observe({
      if (any(class(s_df()) == "data.frame" | class(s_df()) == "sf") & ("layer_disp_name_idx" %in% names(s_df()))) {
        choices <- unique(s_df()$layer_disp_name_idx)
      } else if (any(class(s_df()) == "data.frame" | class(s_df()) == "sf") & !("layer_disp_name_idx" %in% names(s_df()))) {
        choices <- names(s_df())
      } else if (class(s_df()) == "character" | class(s_df()) == "numeric" | class(s_df()) == "integer") {
        choices <- s_df()
      } else {
        choices <- ""
      }
      updateSelectInput(session, "single_input", choices = choices)
    })

    return(
      selection <- reactive({
        input$single_input
      })
    )
  })
}

## To be copied in the UI
# mod_single_input_ui("single_input_ui_1")

## To be copied in the server
# callModule(mod_single_input_server, "single_input_ui_1")
