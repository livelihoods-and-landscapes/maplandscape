
# waiting screen spinners

map_screen <- shiny::tagList(
  waiter::spin_flower(),
  shiny::h4("Drawing map...")
)

join_screen <- shiny::tagList(
  waiter::spin_flower(),
  shiny::h4("Joining tables...")
)

sync_screen <- shiny::tagList(
  waiter::spin_flower(),
  shiny::h4("Syncing forms...")
)

resize_screen <- shiny::tagList(
  waiter::spin_flower(),
  shiny::h4("Resizing...")
)

delete_screen <- shiny::tagList(
  waiter::spin_flower(),
  shiny::h4("Removing rows...")
)

edit_screen <- shiny::tagList(
  waiter::spin_flower(),
  shiny::h4("Saving edits...")
)

sync_edit_screen <- shiny::tagList(
  waiter::spin_flower(),
  shiny::h4("Syncing edits...")
)
