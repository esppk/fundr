#' pivot_view UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @import excelR purrr stringr
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
        tags$br(),
        actionBttn(
                inputId = ns("pull"),
                label = "Pull", 
                style = "fill",
                color = "warning",
                block = TRUE
              ),
        pickerInput(
          inputId = ns("firm_name"),
          label = "Select Firm",
          choices = candidates,
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
        excelOutput(ns("tbl"), height = "100%"),
        tags$br(),
        actionBttn(
          inputId = ns("delete"),
          label = "DELETE", 
          style = "material-flat",
          color = "danger",
          block = TRUE
        ),
        tags$br(),
        uiOutput(ns("pagebtns"))
      )
    )
  )
}
    
#' pivot_view Server Function
#'
#' @noRd 
mod_pivot_view_server <- function(input, output, session, db, db2){
  ns <- session$ns
  
  type_filter <- reactiveValues()
  
  type_filter$status <- FALSE
  
  data <- eventReactive({
      input$pull | input$confirm
    }, {
    
      dat <- db$find()

      # req(data()$stock_name)

      if(!is.null(dat)) {
        dat %>%
          select(stock_name, abbr, date, name, first, coupon, tenure, total) %>%
          mutate_at(vars(coupon, tenure, total), as.numeric) %>% 
          mutate(date = lubridate::ymd(date))
      }
    # golem::print_dev(dat)
    # fct_loadr()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # observe({
  #   
  #   req(data()$stock_name)
  # 
  #   
  #   updatePickerInput(
  #     session,
  #     inputId = "firm_name",
  #     selected = NULL,
  #     choices = candidates$stock_name
  #     # choices = data()$stock_name %>% unique() %>% na.omit()
  #   )
  # })
  #   
  
  output$vital <- renderTable({
    
    req(data()$stock_name)
    
    # golem::print_dev(data() %>% filter(stock_name == input$firm_name))
    
    # browser()
    data() %>%
      filter(stock_name == input$firm_name) %>%
      group_by(stock_name) %>%
      summarise(weight_cost = weighted.mean(coupon, total, na.rm = TRUE),
                n = n(), sum = sum(total, na.rm = TRUE))

    
  })

  
  summary_tbl <- reactive({
    
    req(data()$stock_name)
    
    base_tbl <- data() %>% 
      filter(month(date) >= which(month.abb == input$month[1]),
             month(date) <= which(month.abb == input$month[2])) %>% 
      group_by(stock_name, first) %>%
      summarise(n = n(), sum = sum(total))
    
    if(!type_filter$status){
      
      base_tbl
      
    } else {
      base_tbl %>% 
        filter(.data$first == type_filter$type)
    }
    
  })
  
  output$cards <- renderUI({
    
    req(data()$stock_name)
    
    firm_tbl <- summary_tbl() %>% filter(stock_name == input$firm_name)
    
    nrows <- nrow(firm_tbl)
    
    # golem::print_dev(firm_tbl %>% slice(1) %>% pull("first"))
  
    if(nrows > 0){
      
      map(1:nrows, ~ material_card(
        title = firm_tbl %>% slice(.x) %>% pull("first"),
        depth = 3,
        id = firm_tbl %>% slice(.x) %>% pull("first"),
        divider = TRUE,
        color = "lime",
        shiny::tags$h5(firm_tbl %>% slice(.x) %>% pull("sum") %>% paste0(., "亿")),
        shiny::tags$h5(firm_tbl %>% slice(.x) %>% pull("n") %>% paste0(., "起"))
      ))
    }

  })
 
  output$tbl <- renderExcel({
    
    req(data()$stock_name)
    
    cols <- data.frame(
      title = c("abbr", "date", "name", "first", "coupon", "tenure", "total", "delete"),
      width = rep(200, 8),
      type = c("text", "date", "text", "text", "number", "number", "number", "dropdown"),
      source = I(list(NA, NA,NA,NA,NA,NA,NA, c("del", "keep")))
    )
    
    excelTable(data() %>% filter(stock_name == input$firm_name) %>% 
                 filter(month(date) >= which(month.abb == input$month[1]),
                        month(date) <= which(month.abb == input$month[2])) %>% 
                 select(-stock_name) %>% mutate(delete = "keep"),
               columns = cols, 
               search=TRUE, pagination = 10,
               autoFill = TRUE)
  })
 
  n_name <- reactiveVal(0)
  
  sorted_names <- reactive({
    
    req(data()$stock_name)
    
    data() %>% 
      filter(month(date) >= which(month.abb == input$month[1]),
             month(date) <= which(month.abb == input$month[2])) %>% 
      semi_join(candidates, by = "stock_name") %>% 
      group_by(stock_name) %>% 
      summarise(sum = sum(total, na.rm = TRUE)) %>% 
      arrange(desc(sum)) %>% 
      pull("stock_name") %>% na.omit()
  })
  

  
  output$pagebtns <- renderUI({
    
    req(data())
    
    fluidRow(
      col_6(
        actionBttn(
          inputId = ns("prev"),
          label = "Prev", 
          style = "float",
          color = "success"
        )
      ),
      col_6(
        actionBttn(
          inputId = ns("next_"),
          label = "Next",
          style = "float",
          color = "success"
        )
      )
    )

  })
  
  observeEvent(input$prev, {
    
    if(n_name() <= 1){
      
      sendSweetAlert(
        session = session,
        title = "First Page Already !!",
        text = "",
        type = "info")
    } else {
      
      
      n = n_name()
      
      n_name(n-1)
      
      updatePickerInput(
        session,
        inputId = "firm_name",
        selected = sorted_names()[n-1]
        # choices = data()$stock_name %>% unique() %>% na.omit()
      )
      
    }
    
    
  })
  
  
  observeEvent(input$next_, {
    
    if(n_name() >= length(sorted_names())){
      
      sendSweetAlert(
        session = session,
        title = "Last Page Already !!",
        text = "",
        type = "info")
    } else {
      
      n = n_name()
      
      n_name(n+1)
      
      updatePickerInput(
        session,
        inputId = "firm_name",
        selected = sorted_names()[n+1]
        # choices = data()$stock_name %>% unique() %>% na.omit()
      )
      

    }
    
    
  })
  
  observeEvent(input$delete, {
    
    confirmSweetAlert(
      session = session,
      inputId = "confirm",
      title = "Comfirm delete ?"
    )
  })
  
  
  
  observeEvent(input$confirm, {
    
    changed <- excel_to_R(input$tbl)
    
    
    del_str <- changed %>%  
      filter(delete == "del") %>% 
      # aprilBond %>% 
      # slice(1, 2) %>% 
      str_glue_data('{{"abbr": "{abbr}", "date": "{date}", "first": "{first}"}}')  
    
    
    if (input$confirm) {
      walk(del_str, ~ db$remove(.))
    }
    
    
  })
  
  # observeEvent(input$tbl, {
  #   
  #   changed <- excel_to_R(input$tbl)
  #   
  #   if(!is.null(changed)){
  #   
  #     golem::print_dev(changed)
  #   }
  #   
  # })
}
    
## To be copied in the UI
# mod_pivot_view_ui("pivot_view_ui_1")
    
## To be copied in the server
# callModule(mod_pivot_view_server, "pivot_view_ui_1")
 
