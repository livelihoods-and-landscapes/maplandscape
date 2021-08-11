#' @export
mod_multiple_input_UI <- function(id, label) {
  tagList(
    selectInput(NS(id, "multiple_input"), label, choices = NULL, multiple = TRUE)
  )
}

#' @export
mod_multiple_input_Server <- function(id, m_df) {
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

      updateSelectInput(
        session,
        "multiple_input",
        choices = choices,
        selected = NULL
        )
    })

    return(
      multiple_selection <- reactive({
        input$multiple_input
      })
    )
  })
}


