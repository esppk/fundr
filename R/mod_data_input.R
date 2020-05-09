#' data_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @import dplyr stringr
#' 
#' @importFrom shinyWidgets sendSweetAlert actionBttn
#'
#' @importFrom shiny NS tagList 
#' 
mod_data_input_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      h1("Data Input Here"),
      fluidRow(
        col_4(fileInput(ns("file"), label = "Upload File"),
              actionBttn(
                inputId = ns("append"),
                label = "Append", 
                style = "fill",
                color = "success"
              )),
        col_6(DT::DTOutput(ns("preview")))
      )
    )
  )
}
    
#' data_input Server Function
#'
#' @noRd 
mod_data_input_server <- function(input, output, session, db){
  
  ns <- session$ns
  
  data <- reactive({
    
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    
    switch(ext,
           
           xlsx = readxl::read_excel(input$file$datapath),
           csv = readr::read_csv(input$file$datapath),
           validate("Invaldi file; please upload either csv or xlsx.")
    )
  })
  
  observeEvent(input$append, {
    
    tryCatch({
      
      db$drop()
      
      data() %>%
        mutate_at(vars(total, planned, tenure, coupon), as.numeric) %>% 
        mutate(code = str_extract(stocks, "[0-9]+(?=\\()")) %>% 
        mutate(code = case_when(
          nchar(code) == 4 ~ str_c(code, ".HK"),
          str_detect(code, "^6") ~ str_c(code, ".SH"),
          TRUE ~ str_c(code, ".SZ")
        )) %>% 
        mutate(stock_name = str_extract(stocks, "(?<=[0-9]{4,6}\\()[^\\(\\)]+(?=\\))")) %>% 
        db$insert(.)
      
      sendSweetAlert(
        session = session,
        title = "Success !!",
        text = "All in order",
        type = "success")
      
    },
    error = function(e) {
      golem::print_dev(e)
      sendSweetAlert(
        session = session,
        title = "Error !!",
        text = "Error parsing the colnames; Please make sure your have column `firm`",
        type = "error"
      )
    })
  })
  
  output$preview <- DT::renderDT({

    data()
  })
  
 
}
    
## To be copied in the UI
# mod_data_input_ui("data_input_ui_1")
    
## To be copied in the server
# callModule(mod_data_input_server, "data_input_ui_1")
 
