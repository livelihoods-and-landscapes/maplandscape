library(magrittr)
library(shiny)

# Google OAuth2.0 app
if (interactive()) {
  # testing url
  options(shiny.port = 8100)
  APP_URL <- "http://localhost:8100/"
} else {
  # deployed URL
  APP_URL <- "https://map.livelihoods-and-landscape.com"
}

# Note that secret is not really secret, and it's fine to include inline
app <- httr::oauth_app("shiny",
                       key = "25228130184-l3g7lpo3b7vblf2orrc4it8i2r9p1dff.apps.googleusercontent.com",
                       secret = "7lYDgQmFpZFDCI6Zr1L27gf9",
                       redirect_uri = APP_URL
)

# Google OAugh endpoint
api <- httr::oauth_endpoints("google")

# Always request the minimal scope needed.
scope <- "https://www.googleapis.com/auth/devstorage.read_write"

url <- httr::oauth2.0_authorize_url(api, app, scope = scope)
