# updage packages
renv::update()

# snapshop current state of package dependencies in the lockfile after updating packages
renv::snapshot()

# update packages when updating R
options(configure.args = c(sf="--with-proj-include=/usr/local/include/"))

renv::install(
  c(
    "bslib",
    "config",
    "dplyr",
    "DT",
    "fs",
    "ggplot2",
    "httr",
    "jsonlite",
    "leaflet",
    "lubridate",
    "magrittr",
    "purrr",
    "RColorBrewer",
    "readr",
    "rlang",
    "sf",
    "shiny",
    "shinyFeedback",
    "shinyjs",
    "stringr",
    "tibble",
    "tidyr",
    "tidyselect",
    "waiter",
    "xfun",
    "grid",
    "DBI",
    "RPostgres",
    "leaflet",
    "styler",
    "devtools"
  )
)

renv::install("livelihoods-and-landscapes/leafgl")
renv::install("livelihoods-and-landscapes/qfieldcloudR")

