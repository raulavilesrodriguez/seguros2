appendDonate <- function(db, input){
  quary <- 1
  millasCliente <- dbGetQuery(db, sprintf("SELECT millas 
                                     FROM responses_df 
                                     WHERE nombre = '%s';", input$cliente))
  
  millasNetas <- as.numeric(millasCliente) - input$millasrecibidas
  
  millasBeneficiario <- dbGetQuery(db, sprintf("SELECT millasrecibidas 
                                     FROM beneficiario_df 
                                    WHERE nombre = '%s';", input$beneficiario))
  
  millasBeneficiario <- ifelse(is.na(millasBeneficiario), 0 , as.numeric(millasBeneficiario))
  
  # the values to setup benefcliente table
  row_id <- dbGetQuery(db, sprintf("SELECT row_id 
                                     FROM responses_df 
                                     WHERE nombre = '%s';", input$cliente))
  beneficiario_id <- dbGetQuery(db, sprintf("SELECT beneficiario_id 
                                     FROM beneficiario_df 
                                    WHERE nombre = '%s';", input$beneficiario))
  

  print(millasBeneficiario)
  
  # Only is possible positive values
  if(millasNetas >= 0 & input$millasrecibidas>0){
    dbExecute(db, sprintf("UPDATE responses_df 
                          SET millas = '%s'
                          WHERE nombre = '%s';", as.numeric(millasNetas), input$cliente))
    
    millasBeResult <- as.numeric(millasBeneficiario) + input$millasrecibidas
      
    dbExecute(db, sprintf("UPDATE beneficiario_df 
                          SET millasrecibidas = '%s'
                          WHERE nombre = '%s';", as.numeric(millasBeResult), input$beneficiario))
    
    dbExecute(db, sprintf("INSERT INTO
                            benefcliente (beneficiario_id, row_id, millas)
                           VALUES ('%s', '%s', '%s');", beneficiario_id, row_id, input$millasrecibidas))
    quary <- 0
    quary
  }
  quary
}