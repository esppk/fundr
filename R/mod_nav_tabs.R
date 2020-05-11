#' nav_tabs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @import mongolite
#'
#' @importFrom shiny NS tagList 
mod_nav_tabs_ui <- function(id){
  ns <- NS(id)
  tagList(
    material_side_nav(
      fixed = TRUE,
      # Place side-nav tabs within side-nav
      material_side_nav_tabs(
        side_nav_tabs = c(
          "Input" = "input_tab",
          "Pivot" = "pivot_tab",
          "manual" = "manual_tab"
        ),
        icons = c("cast", "insert_chart", "edit")
      )
    ),
    # Define side-nav tab content
    material_side_nav_tab_content(
      side_nav_tab_id = "input_tab",
      mod_data_input_ui(ns("data_input_ui_1"))
    ),
    material_side_nav_tab_content(
      side_nav_tab_id = "pivot_tab",
      mod_pivot_view_ui(ns("pivot_view_ui_1"))
    ),
    material_side_nav_tab_content(
      side_nav_tab_id = "manual_tab",
      mod_manual_ui(ns("manual_ui_1"))
    )
  )
}
    
#' nav_tabs Server Function
#'
#' @noRd 
mod_nav_tabs_server <- function(input, output, session){
  ns <- session$ns
  
  db1 <- mongo(collection = "fundr", db = "fundr", 
              url = "mongodb+srv://saler:5al3r@cluster-opuen.mongodb.net/test?retryWrites=true&w=majority", 
              verbose = FALSE)
  
  db2 <- mongo(collection = "extra", db = "fundr", 
              url = "mongodb+srv://saler:5al3r@cluster-opuen.mongodb.net/test?retryWrites=true&w=majority", 
              verbose = FALSE)
  
  callModule(mod_data_input_server, "data_input_ui_1", db = db1, db2 = db2)
  callModule(mod_pivot_view_server, "pivot_view_ui_1", db = db1, db2 = db2)
  callModule(mod_manual_server, "manual_ui_1", db = db1) 
}
    
## To be copied in the UI
# mod_nav_tabs_ui("nav_tabs_ui_1")
    
## To be copied in the server
# callModule(mod_nav_tabs_server, "nav_tabs_ui_1")
 
