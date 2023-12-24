deleteDataBeneficiaries <- function(db, input){
  row_selection <- reactiveValues(
    # This will return an empty data frame
    rows = data.frame()
  )
  
  deleteData <- reactive({
    quary <- lapply(row_selection$rows, function(nr){
      try({
        dbExecute(db, sprintf("DELETE FROM benefcliente WHERE beneficiario_id = '%s';", nr))
      }, silent = TRUE)
      dbExecute(db, sprintf("DELETE FROM beneficiario_df WHERE beneficiario_id = '%s'", nr))
    })
  })
  
  # Delete rows when selected. Otherwise display error message
  observeEvent(input$delete_beneficiario, priority = 20,{
    
    if(length(input$table_beneficiaries_rows_selected)>=1 ){
      SQL_df <- dbReadTable(db, "beneficiario_df")
      row_selection$rows <- SQL_df[input$table_beneficiaries_rows_selected, "beneficiario_id"]
      showModal(
        modalDialog(
                    title = "Advertencia",
                    paste("Estás seguro de borrar el/los beneficiario(s)?"),
                    br(),
                    br(),
                    actionButton("yes_buttonBe", "Si"),
                    actionButton("no_buttonBe", "No"),
                    easyClose = TRUE, footer = NULL))
      
    }
    
    else if(length(input$table_beneficiaries_rows_selected) < 1 ){
      showModal(
        modalDialog(
          title = "Advertencia",
          paste("Por favor selecciona el/los cliente(s)" ),easyClose = TRUE
        ) 
      )
    }
    
  })
  
  observeEvent(input$yes_buttonBe, priority = 20,{
    
    deleteData()
    removeModal()
    
  })
  
  observeEvent(input$no_buttonBe, priority = 20,{
    removeModal()
    
  })
  
}