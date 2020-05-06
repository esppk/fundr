#' pivot_view UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @import excelR purrr
#' 
#' @import dplyr
#' 
#' @importFrom shinydashboard valueBox 
#'
#' @importFrom shiny NS tagList 
mod_pivot_view_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      col_4(
        uiOutput(ns("cards"))
      ),
      col_8(
        
        excelOutput(ns("tbl"))
      )
    )
  )
}
    
#' pivot_view Server Function
#'
#' @noRd 
mod_pivot_view_server <- function(input, output, session){
  ns <- session$ns
  
  data <- reactive({
    
    fct_loadr()
  })
  
  summary_tbl <- reactive({
    
    data() %>% 
      group_by(name, type) %>%
      summarise(n = n(), sum = sum(amount))
  })
  
  output$cards <- renderUI({
    
    firm_tbl <- summary_tbl() %>% filter(name == "大悦城")
    
    nrows <- nrow(firm_tbl)
    
    tagList(!!!map(1:nrows, ~ material_card(
      title = firm_tbl %>% pluck(3, .x),
      depth = 3,
      color = "green",
      shiny::tags$h5(firm_tbl %>% pluck(4, .x))
    )))
    
    
  })
 
  output$tbl <- renderExcel({
    excelTable(data())
  })
  
  
}
    
## To be copied in the UI
# mod_pivot_view_ui("pivot_view_ui_1")
    
## To be copied in the server
# callModule(mod_pivot_view_server, "pivot_view_ui_1")
 
