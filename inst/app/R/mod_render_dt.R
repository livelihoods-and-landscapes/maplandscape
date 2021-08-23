
mod_render_dt_UI <- function(id) {
  tagList(
    DT::dataTableOutput(NS(id, "data_table"))
  )
}


mod_render_dt_Server <- function(id, dt, editable) {
  moduleServer(id, function(input, output, session) {
    output$data_table <- DT::renderDataTable({
      req(dt())

      dt <- dt()

      if ("sf" %in% class(dt)) {
        dt <- dt %>%
          sf::st_drop_geometry() %>%
          as.data.frame()
      }

      DT::datatable(
        data = dt,
        editable = editable
      )
    })
  })
}
