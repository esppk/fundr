#' data_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#'
#' @importFrom shiny NS tagList 
mod_data_input_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Data Input Here"),
    fluidPage(
      fluidRow(
        col_4(fileInput(ns("file"), label = "Upload File")),
        col_6(DT::DTOutput(ns("preview")))
      )
    )
  )
}
    
#' data_input Server Function
#'
#' @noRd 
mod_data_input_server <- function(input, output, session){
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
  
  output$preview <- DT::renderDT({

    data()
  })
  
 
}
    
## To be copied in the UI
# mod_data_input_ui("data_input_ui_1")
    
## To be copied in the server
# callModule(mod_data_input_server, "data_input_ui_1")
 
