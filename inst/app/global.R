# Google OAuth2.0 app
if (interactive()) {
  # testing url
  options(shiny.port = 8100)
  APP_URL <- "http://localhost:8100/"
} else {
  # deployed URL
  APP_URL <- "https://livelihoods-landscape.shinyapps.io/maplandscape"
}

# Note that secret is not really secret, and it's fine to include inline
app <- httr::oauth_app("shiny",
                       key = "252184882311-48os9c1p3kibne8pkck9euspctsgl7u9.apps.googleusercontent.com",
                       secret = "MLsy5lBi072xIBGmAZpPgHuE",
                       redirect_uri = APP_URL
)

# Google OAugh endpoint
api <- httr::oauth_endpoints("google")

# Always request the minimal scope needed.
scope <- "https://www.googleapis.com/auth/devstorage.read_write"

url <- httr::oauth2.0_authorize_url(api, app, scope = scope)
