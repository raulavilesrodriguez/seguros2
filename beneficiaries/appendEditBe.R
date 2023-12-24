appendEditBe <- function(db, input){
  SQL_df <- dbReadTable(db, "beneficiario_df")
  row_selection <- SQL_df[input$table_beneficiaries_row_last_clicked, "beneficiario_id"]
  
  # Change to UPPER the name variable and eliminate Spaces
  newName <- changeUpper(input$nombreBeE)
  newCedula <- onlyNumbers(input$cedulaBeE)
  
 
  unica_cedula <- dbGetQuery(db, sprintf("SELECT beneficiario_id FROM  beneficiario_df
                                           WHERE cedula = '%s' AND
                                           beneficiario_id != '%s';", newCedula, row_selection))
  
  unico_nombre <- dbGetQuery(db, sprintf("SELECT beneficiario_id FROM beneficiario_df
                                           WHERE nombre = '%s' AND
                                           beneficiario_id != '%s';", newName, row_selection))
  
  print(nrow(unica_cedula))
  print(nrow(unico_nombre))
  
  if(nrow(unica_cedula)==0 & nrow(unico_nombre) == 0){
    
    dbExecute(db, sprintf("UPDATE beneficiario_df SET nombre = $1, sexo = $2, 
                          edad = $3, millasrecibidas = $4, cedula = $5, email = $6,
                          estado = $7 WHERE beneficiario_id = '%s'", row_selection), 
              param = list(newName,
                           input$sexoBeE,
                           input$edadBeE,
                           input$millasrecibidasE,
                           newCedula,
                           input$emailBeE,
                           input$estadoBeE))
    removeModal()
  } else {
    showModal(
      modalDialog(
        title = "Advertencia. Cambios no grabados",
        paste("No pueden haber 2 Beneficiarios con la misma cédula o el mismo nombre" ),easyClose = TRUE
      )
    )
  }
  
}