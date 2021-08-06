mod_render_dt_UI <- function(id) {
  tagList(
    DT::dataTableOutput(NS(id, "data_table"))
  )
}

mod_render_dt_Server <- function(id, dt, editable) {
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
