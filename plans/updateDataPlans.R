updateDataPlans <- function(db, input, session, labelMandatory){
  SQL_df <- dbReadTable(db, "planes")
  
  
  showModal(
    if(length(input$table_planes_rows_selected) > 1 ){
      modalDialog(
        title = "Advertencia",
        paste("Por favor selecciona solo un Plan" ),easyClose = TRUE, footer = modalButton("Cerrar"))
    } else if(length(input$table_planes_rows_selected) < 1){
      modalDialog(
        title = "Advertencia",
        paste("Por favor selecciona un Plan" ),easyClose = TRUE, footer = modalButton("Cerrar"))
    })  
  
  if(length(input$table_planes_rows_selected) == 1 ){
    
    entry_formPlans("submit_editPlan", db, labelMandatory)
    
    # to update data
    updateTextInput(session, "nombrePlan", value = SQL_df[input$table_planes_rows_selected, "nombre_plan"])
    
    
  }
}