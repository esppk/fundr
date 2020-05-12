#' manual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @import excelR dplyr
#' 
#' @importFrom shinyWidgets actionBttn
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_manual_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      col_4(       
        pickerInput(
          inputId = ns("firm_name"),
          label = "Select Firm", 
          choices = candidates$stock_name,
          options = list(
            `live-search` = TRUE)
        )),
      col_8(
        tags$br(),
        excelOutput(ns("manual")),
        actionBttn(
          inputId = ns("push"),
          label = "Push", 
          style = "fill",
          color = "danger"
        )
      )
    )
  )
}
    
#' manual Server Function
#'
#' @noRd 
mod_manual_server <- function(input, output, session, db){
  ns <- session$ns
  

  output$manual <- renderExcel({
    
    tbl <- tibble(
      abbr = "",
      date = "",
      first = "",
      total = "",
      coupon = "",
      tenure = "",
      currency = ""
    )
    
    excelTable(tbl)
  })
  
  observeEvent(input$push, {
    
    changed <- excel_to_R(input$manual)
    
    # golem::print_dev(colnames(changed))
    
    db$insert(changed %>%
                mutate(stock_name = input$firm_name) %>% 
                select(stock_name, abbr, date, first, total, coupon, tenure) %>%
                mutate_all( as.character) %>%
                mutate_at(vars(total, coupon, tenure), as.numeric)
    )
    
    
    

    sendSweetAlert(
      session = session,
      title = "Success !!",
      text = "Data has been uploaded",
      type = "success"
    )


    
  })
  
  
  
  
}
    
## To be copied in the UI
# mod_manual_ui("manual_ui_1")
    
## To be copied in the server
# callModule(mod_manual_server, "manual_ui_1")
 
