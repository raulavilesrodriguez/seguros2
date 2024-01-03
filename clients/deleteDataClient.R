deleteDataClient <- function(db, input){
  row_selection <- reactiveValues(
    # This will return an empty data frame
    rows = data.frame()
  )
  
  deleteData <- reactive({
    quary <- lapply(row_selection$rows, function(nr){
      dbExecute(db, sprintf("DELETE FROM clientePlanes WHERE row_id = '%s';", nr))
      dbExecute(db, sprintf("DELETE FROM benefcliente WHERE row_id = '%s';", nr))
      dbExecute(db, sprintf("DELETE FROM responses_df WHERE row_id = '%s'", nr))
    })
  })
  
  # Delete rows when selected. Otherwise display error message
  observeEvent(input$delete_button, priority = 20,{
    
    if(length(input$responses_table_rows_selected)>=1 ){
      SQL_df <- dbReadTable(db, "responses_df")
      row_selection$rows <- SQL_df[input$responses_table_rows_selected, "row_id"]
      showModal(
        modalDialog(id="delete_modal",
                    title = "Advertencia",
                    paste("Estás seguro de borrar el/los cliente(s)?"),
                    br(),
                    br(),
                    actionButton("yes_button", "Si"),
                    actionButton("no_button", "No"),
                    easyClose = TRUE, footer = NULL))
      
    }
    
    else if(length(input$responses_table_rows_selected) < 1 ){
      showModal(
        modalDialog(
          title = "Advertencia",
          paste("Por favor selecciona el/los cliente(s)" ),easyClose = TRUE,
          footer = modalButton("Cerrar")
        ) 
      )
    }
    
  })
  
  observeEvent(input$yes_button, priority = 20,{
    
    deleteData()
    removeModal()
    
  })
  
  observeEvent(input$no_button, priority = 20,{
    removeModal()
    
  })
  
}