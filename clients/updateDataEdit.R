updateDataEdit <- function(db, input, session, labelMandatory){
  
  SQL_df <- dbReadTable(db, "responses_df")
  
  showModal(
    if(length(input$responses_table_rows_selected) > 1 ){
      modalDialog(
        title = "Advertencia",
        paste("Por favor selecciona solo un cliente" ),
        easyClose = TRUE, footer = modalButton("Cerrar"))
    } else if(length(input$responses_table_rows_selected) < 1){
      modalDialog(
        title = "Advertencia",
        paste("Por favor selecciona un cliente" ),
        easyClose = TRUE, footer = modalButton("Cerrar"))
    })  
  
  if(length(input$responses_table_rows_selected) == 1 ){
    
    entry_form("submit_edit", db, labelMandatory)
    
    # to select planes of an client
    row_selection <- SQL_df[input$responses_table_rows_selected, "row_id"]
    SQL_planes <- dbReadTable(db, "planes")
    planes_selected <- dbGetQuery(db, 
                                  sprintf(
                                    "SELECT 
                                  	p.nombre_plan
                                  FROM 
                                  	planes p
                                  INNER JOIN clienteplanes c
                                  	ON p.planes_id = c.planes_id
                                  WHERE c.row_id = '%s';", 
                                    row_selection
                                  ))
    
    banana <- c(planes_selected[['nombre_plan']])
    
    updateTextInput(session, "nombre", value = SQL_df[input$responses_table_rows_selected, "nombre"])
    updateSelectInput(session, "sexo", selected = SQL_df[input$responses_table_rows_selected, "sexo"])
    updateSliderInput(session, "edad", value = SQL_df[input$responses_table_rows_selected, "edad"])
    updateNumericInput(session, "millas", value = SQL_df[input$responses_table_rows_selected, "millas"])
    updateTextInput(session, "cedula", value = SQL_df[input$responses_table_rows_selected, "cedula"])
    updateTextInput(session, "email", value = SQL_df[input$responses_table_rows_selected, "email"])
    updateSelectizeInput(session, "planes", 'Planes', SQL_planes[,"nombre_plan"], selected = banana)
    updateTextAreaInput(session, "comentario", value = SQL_df[input$responses_table_rows_selected, "comentario"])
    updateDateInput(session, "dateCliente", value = SQL_df[input$responses_table_rows_selected, "creado"])
    
  }
}