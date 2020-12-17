args <- commandArgs(trailingOnly = TRUE)

deploy_app <- FALSE

# Update global_data
usethis::ui_todo("Checking update for latest_covid dataset...")
source("data-raw/latest_covid.R")

if(deploy_app) {
  remotes::install_deps(
    dependencies = TRUE,
    upgrade = "never",
    repos = "https://packagemanager.rstudio.com/all/__linux__/bionic/latest"
  )
  
  rsconnect::setAccountInfo(
    name = 'apmuhamilton',
    token = args[1],
    secret= args[2]
  )
  
  files <- list.files('.')
  files <- files[files != 'data-raw']
  
  rsconnect::deployApp(
    appFiles = files,
    appName = 'hamiltonRestIreland',
    forceUpdate = TRUE,
    account = 'apmuhamilton'
  )
  
} else {
  message("Nothing to deploy.")
}
