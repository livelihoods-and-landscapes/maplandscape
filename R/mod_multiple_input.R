#' multiple_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_multiple_input_ui <- function(id, label) {
  tagList(
    selectInput(NS(id, "multiple_input"), label, choices = NULL, multiple = TRUE)
  )
}

#' multiple_input Server Function
#'
#' @noRd
mod_multiple_input_server <- function(id, m_df) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(m_df())

      if (any(class(m_df()) == "data.frame" | class(m_df()) == "sf")) {
        choices <- names(m_df())
      } else if (class(m_df()) == "character" | class(m_df()) == "numeric" | class(m_df()) == "integer") {
        choices <- m_df()
      } else {
        choices <- ""
      }

      updateSelectInput(session, "multiple_input", choices = choices, selected = NULL)
    })

    return(
      multiple_selection <- reactive({
        input$multiple_input
      })
    )
  })
}

## To be copied in the UI
# mod_multiple_input_ui("multiple_input_ui_1")

## To be copied in the server
# callModule(mod_multiple_input_server, "multiple_input_ui_1")
