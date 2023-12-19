# Function to append data to the SQL table
appendDataBe <- function(data, db, input){
  consulting <- glue("SELECT * FROM beneficiario_df
                        WHERE cedula = '{input$cedulaBe}';")
  cedula_unica <- dbGetQuery(db, consulting)
  quary <- 1
  if(nrow(cedula_unica) == 0){
    dbWriteTable(db, "beneficiario_df", data, append = TRUE)
    
    dbExecute(db, sprintf("UPDATE responses_df 
                          SET beneficiario_id = '%s'
                          WHERE nombre = '%s';", data[['beneficiario_id']], input$cliente))
    
    millasCliente <- dbGetQuery(db, sprintf("SELECT millas 
                                     FROM responses_df 
                                     WHERE nombre = '%s';", input$cliente))
    millasNetas <- millasCliente - data[['millasrecibidas']]
    
    print(class(millasNetas))
    
    dbExecute(db, sprintf("UPDATE responses_df 
                          SET millas = '%s'
                          WHERE nombre = '%s';", as.numeric(millasNetas), input$cliente))
    
    quary <- 0
    quary
  }
  quary
}
