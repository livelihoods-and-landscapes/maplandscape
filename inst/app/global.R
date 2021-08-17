library(magrittr)
library(shiny)

# Google OAuth2.0 app
if (interactive()) {
  # testing url
  options(shiny.port = 3838)
  APP_URL <- "http://localhost:3838/"
} else {
  # deployed URL
  APP_URL <- config::get("app_url")
}

# Note that secret is not really secret, and it's fine to include inline
app <- httr::oauth_app("shiny",
                       key = config::get("oauth_key"),
                       secret = config::get("oauth_secret"),
                       redirect_uri = APP_URL
)

# Google OAugh endpoint
api <- httr::oauth_endpoints("google")

# Always request the minimal scope needed.
scope <- "https://www.googleapis.com/auth/devstorage.read_write"

url <- httr::oauth2.0_authorize_url(api, app, scope = scope)
