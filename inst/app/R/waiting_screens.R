# waiting screen spinners

map_screen <- shiny::tagList(
  waiter::spin_flower(),
  shiny::h4("Drawing map...")
)

join_screen <- shiny::tagList(
  waiter::spin_flower(),
  shiny::h4("Joining layers...this could take a while!")
)

login_screen <- shiny::tagList(
  waiter::spin_flower(),
  shiny::h4("Logging in...")
)

project_screen <- shiny::tagList(
  waiter::spin_flower(),
  shiny::h4("Getting projects...")
)

download_screen <- shiny::tagList(
  waiter::spin_flower(),
  shiny::h4("Downloading...")
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

report_screen <- shiny::tagList(
  waiter::spin_flower(),
  shiny::h4("Generating report...")
)
