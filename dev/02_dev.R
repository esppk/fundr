# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "shinymaterial" )
usethis::use_package( "readr" )
usethis::use_package( "purrr" )
usethis::use_package( "readxl" )
usethis::use_package( "DT" )
usethis::use_package( "excelR" )
usethis::use_package( "shinyWidgets" )
usethis::use_package( "shinyjs" )
usethis::use_package( "lubridate" )
usethis::use_package( "mongolite" )
usethis::use_package( "stringr" )
usethis::use_package( "tidyr" )
usethis::use_package( "waiter" )




## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "nav_tabs" ) # Name of the module
golem::add_module( name = "data_input" ) # Name of the module
golem::add_module( name = "pivot_view" )
golem::add_module( name = "manual" )

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "loadr" ) 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "firm_names", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("fundr")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

