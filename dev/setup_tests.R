#set up snapshot testing

library(devtools)
library(shinytest)
install_github("livelihoods-and-landscapes/maplandscape")
library(maplandscape)
shinytest::recordTest(ShinyDriver$new(path = "inst/app/"))
