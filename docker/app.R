# run maplansdscape app
dir <- system.file("app", package = "maplandscape")
setwd(dir)
library(maplandscape)
shiny::shinyAppDir(".")
