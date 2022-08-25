library(qfieldcloudR)
library(maplandscape)
library(magrittr)
library(shiny)


# get config variables for postgis
dbname = config::get("postgis_dbname")
user = config::get("postgis_username")
password = config::get("postgis_password")
host = config::get("postgis_host")
port = config::get("postgis_port")
