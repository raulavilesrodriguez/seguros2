appendEdit <- function(db, input){
  SQL_df <- dbReadTable(db, "responses_df")
  row_selection <- SQL_df[input$responses_table_row_last_clicked, "row_id"]
  
  # Change to UPPER the name variable and eliminate Spaces
  newName <- changeUpper(input$nombre)
  newCedula <- onlyNumbers(input$cedula)
  
  unica_cedula <- dbGetQuery(db, sprintf("SELECT  row_id FROM  responses_df
                                           WHERE cedula = '%s' AND
                                           row_id != '%s';", newCedula, row_selection))
  
  unico_nombre <- dbGetQuery(db, sprintf("SELECT row_id FROM responses_df
                                           WHERE nombre = '%s' AND
                                           row_id != '%s';", newName, row_selection))
  
  
  if(nrow(unica_cedula)==0 & nrow(unico_nombre) == 0){
    # CHANGE IN IF to not duplicate the planes chosen
    dbExecute(db, sprintf("DELETE FROM clientePlanes
                           WHERE row_id = '%s';", row_selection))
    
    sapply(input$planes, function(p){
      if(!is.null(p)){
        planes_id <- dbGetQuery(db, 
                                sprintf(
                                  "SELECT planes_id FROM planes 
                                     WHERE nombre_plan = '%s';", p
                                ))
        
        dbExecute(db, sprintf("INSERT INTO
                                  clientePlanes (row_id, planes_id)
                                VALUES ('%s', '%s');", row_selection, planes_id)) 
      }
    })
    
    dbExecute(db, sprintf("UPDATE responses_df SET nombre = $1, sexo = $2, 
                          edad = $3, millas = $4, cedula = $5, email = $6,
                          comentario = $7 WHERE row_id = '%s'", row_selection), 
              param = list(newName,
                           input$sexo,
                           input$edad,
                           input$millas,
                           newCedula,
                           input$email,
                           input$comentario))
    removeModal()
  } else {
    showModal(
      modalDialog(
        title = "Advertencia. Cambios no grabados",
        paste("No pueden haber 2 clientes con la misma cédula o el mismo nombre" ),easyClose = TRUE
      )
    )
  }
  
}