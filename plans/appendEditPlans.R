appendEditPlans <- function(db, input){
  SQL_df <- dbReadTable(db, "planes")
  
  row_selection <- SQL_df[input$table_planes_row_last_clicked, "planes_id"]
  
  # Change to UPPER the name variable and eliminate Spaces
  newName <- changeUpper(input$nombrePlan)
  
  unico_nombre <- dbGetQuery(db, sprintf("SELECT planes_id FROM planes
                                           WHERE nombre_plan = '%s' AND
                                           planes_id != '%s';", newName, row_selection))
  
  
  if(nrow(unico_nombre)==0){
    
    dbExecute(db, sprintf("UPDATE planes SET nombre_plan =$1 
                          WHERE planes_id = '%s';", row_selection),
              param = list(
                newName
              ))
    removeModal()
    
  } else {
    showModal(
      modalDialog(
        title = "Advertencia. Cambios no grabados",
        paste("No pueden haber 2 Planes con el mismo nombre" ),easyClose = TRUE,
        footer = modalButton("Cerrar")
      )
    )
  }
  
  
}