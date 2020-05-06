#' pivot_view UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @import excelR
#'
#' @importFrom shiny NS tagList 
mod_pivot_view_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Data Input Here"),
    fluidPage(
      fluidRow(
        col_4(fileInput(ns("file"), label = "Upload File")),
        col_6(excelOutput(ns("preview")))
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
    
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    
    switch(ext,
           
           xlsx = readxl::read_excel(input$file$datapath),
           csv = readr::read_csv(input$file$datapath),
           validate("Invaldi file; please upload either csv or xlsx.")
    )
  })
  
  output$preview <- renderExcel({

    excelTable(data = data(), pagination=10)
  })
  
 
}
    
## To be copied in the UI
# mod_pivot_view_ui("pivot_view_ui_1")
    
## To be copied in the server
# callModule(mod_pivot_view_server, "pivot_view_ui_1")
 
