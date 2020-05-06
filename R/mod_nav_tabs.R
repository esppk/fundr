#' nav_tabs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
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
          "Example Side-Nav Tab 1" = "example_side_nav_tab_1",
          "Example Side-Nav Tab 2" = "example_side_nav_tab_2"
        ),
        icons = c("cast", "insert_chart")
      )
    ),
    # Define side-nav tab content
    material_side_nav_tab_content(
      side_nav_tab_id = "example_side_nav_tab_1",
      mod_data_input_ui(ns("data_input_ui_1"))
    ),
    material_side_nav_tab_content(
      side_nav_tab_id = "example_side_nav_tab_2",
      mod_pivot_view_ui(ns("pivot_view_ui_1"))
    )
  )
}
    
#' nav_tabs Server Function
#'
#' @noRd 
mod_nav_tabs_server <- function(input, output, session){
  ns <- session$ns
  
  callModule(mod_data_input_server, "data_input_ui_1")
  callModule(mod_pivot_view_server, "pivot_view_ui_1")
}
    
## To be copied in the UI
# mod_nav_tabs_ui("nav_tabs_ui_1")
    
## To be copied in the server
# callModule(mod_nav_tabs_server, "nav_tabs_ui_1")
 
