updateBeneficiaries <- function(db, input, session, labelMandatory){
  SQL_df <- dbReadTable(db, "beneficiario_df")
  
  showModal(
    if(length(input$table_beneficiaries_rows_selected) > 1 ){
      modalDialog(
        title = "Advertencia",
        paste("Por favor selecciona solo un Beneficiario" ),
        easyClose = TRUE, footer = modalButton("Cerrar"))
    } else if(length(input$table_beneficiaries_rows_selected) < 1){
      modalDialog(
        title = "Advertencia",
        paste("Por favor selecciona un Beneficiario" ),
        easyClose = TRUE, footer = modalButton("Cerrar"))
    })  
  
  if(length(input$table_beneficiaries_rows_selected) == 1 ){
    
    edit_formBe("submit_edit_Be", db, labelMandatory)
    
    # to select planes of an client
    row_selection <- SQL_df[input$table_beneficiaries_rows_selected, "beneficiario_id"]
    
    
    updateTextInput(session, "nombreBeE", value = SQL_df[input$table_beneficiaries_rows_selected, "nombre"])
    updateSelectInput(session, "sexoBeE", selected = SQL_df[input$table_beneficiaries_rows_selected, "sexo"])
    updateSliderInput(session, "edadBeE", value = SQL_df[input$table_beneficiaries_rows_selected, "edad"])
    updateNumericInput(session, "millasrecibidasE", value = SQL_df[input$table_beneficiaries_rows_selected, "millasrecibidas"])
    updateTextInput(session, "cedulaBeE", value = SQL_df[input$table_beneficiaries_rows_selected, "cedula"])
    updateTextInput(session, "emailBeE", value = SQL_df[input$table_beneficiaries_rows_selected, "email"])
    updateSelectInput(session, "estadoBeE", selected = SQL_df[input$table_beneficiaries_rows_selected, "estado"])
    
  }
  
  
}