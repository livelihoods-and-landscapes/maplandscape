
mod_single_input_UI <- function(id, label) {
  tagList(
    selectInput(NS(id, "single_input"), label, choices = NULL)
  )
}


mod_single_input_Server <- function(id, s_df) {
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

