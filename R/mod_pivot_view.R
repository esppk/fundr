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
#' @import dplyr shinyWidgets
#' 
#' @importFrom lubridate month
#'
#' @importFrom shiny NS tagList 
mod_pivot_view_ui <- function(id){
  
  ns <- NS(id)
  fluidPage(
    fluidRow(
      col_3(
        pickerInput(
          inputId = ns("firm_name"),
          label = "Select Firm", 
          choices = NULL,
          options = list(
            `live-search` = TRUE)
        ),
        uiOutput(ns("cards"))
      ),
      col_1(),
      col_8(
        tags$br(),
        sliderTextInput(
          inputId = ns("month"),
          label = "Choose a range:", 
          choices = month.abb,
          selected = month.abb[c(1, 12)]
        ),
        tableOutput(ns("vital")),
        excelOutput(ns("tbl"), height = "100%")
      )
    )
  )
}
    
#' pivot_view Server Function
#'
#' @noRd 
mod_pivot_view_server <- function(input, output, session, db){
  ns <- session$ns
  
  type_filter <- reactiveValues()
  
  type_filter$status <- FALSE
  
  data <- reactive({
    
    # db$find()
    fct_loadr()
  })

  observe(
    updatePickerInput(
      session,
      inputId = "firm_name",
      selected = NULL,
      choices = data()$name %>% unique()
    )
  )
  
  output$vital <- renderTable({
    
    data() %>% 
      filter(.data$name == input$firm_name) %>% 
      group_by(name) %>% 
      summarise(weight_cost = weighted.mean(coupon, amount, na.rm = TRUE), 
                n = n(), sum = sum(amount))
  })

  
  summary_tbl <- reactive({
    
    
    base_tbl <- data() %>% 
      filter(month(date) >= which(month.abb == input$month[1]),
             month(date) <= which(month.abb == input$month[2])) %>% 
      group_by(name, type) %>%
      summarise(n = n(), sum = sum(amount))
    
    if(!type_filter$status){
      
      base_tbl
      
    } else {
      base_tbl %>% 
        filter(.data$type == type_filter$type)
    }
    
  })
  
  output$cards <- renderUI({
    
    firm_tbl <- summary_tbl() %>% filter(name == input$firm_name)
    
    nrows <- nrow(firm_tbl)
  
    if(nrows > 0){
      
      tagList(!!!map(1:nrows, ~ material_card(
        title = firm_tbl %>% slice(.x) %>% pull("type"),
        depth = 3,
        id = firm_tbl %>% slice(.x) %>% pull("type"),
        divider = TRUE,
        color = "lime",
        shiny::tags$h5(firm_tbl %>% slice(.x) %>% pull("sum") %>% paste0(., "亿")),
        shiny::tags$h5(firm_tbl %>% slice(.x) %>% pull("n") %>% paste0(., "起"))
      )))
    }

  })
 
  output$tbl <- renderExcel({
    
    cols <- data.frame(
      title = c("Amount", "date", "name", "type", "coupon", "tenure"),
      width = rep(200, 6),
      type = c("number", "text", "date", "text", "number", "number")
    )
    
    excelTable(data() %>% filter(name == input$firm_name) %>% 
                 filter(month(date) >= which(month.abb == input$month[1]),
                        month(date) <= which(month.abb == input$month[2])),
               columns = cols, 
               search=TRUE, pagination = 10,
               autoFill = TRUE)
  })
  
  observeEvent(input$tbl, {
    
    changed <- excel_to_R(input$tbl)
    
    if(!is.null(changed)){
    
      golem::print_dev(changed)
    }
    
  })
}
    
## To be copied in the UI
# mod_pivot_view_ui("pivot_view_ui_1")
    
## To be copied in the server
# callModule(mod_pivot_view_server, "pivot_view_ui_1")
 
