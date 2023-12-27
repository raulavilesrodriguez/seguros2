appendCanje <- function(db, input){
  quary <- 1
  
  millasBeneficiario <- dbGetQuery(db, sprintf("SELECT millasrecibidas 
                                     FROM beneficiario_df 
                                    WHERE nombre = '%s';", input$beneficiarioCj))
  
  millasBeneficiario <- ifelse(is.na(millasBeneficiario), 0 , as.numeric(millasBeneficiario))
  
  # the values to setup diners table
  beneficiario_id <- dbGetQuery(db, sprintf("SELECT beneficiario_id 
                                     FROM beneficiario_df 
                                    WHERE nombre = '%s';", input$beneficiarioCj))
  
  
  print(millasBeneficiario)
  
  millasBeResult <- as.numeric(millasBeneficiario) - input$millascanjeadas
  
  # Only is possible positive values
  if(millasBeResult>=0 & millasBeneficiario > 0 & input$millascanjeadas > 0){
    
    dbExecute(db, sprintf("UPDATE beneficiario_df 
                          SET millasrecibidas = '%s'
                          WHERE nombre = '%s';", as.numeric(millasBeResult), input$beneficiarioCj))
    
    dbExecute(db, sprintf("INSERT INTO
                            diners (beneficiario_id, millascanjeadas)
                           VALUES ('%s', '%s');", beneficiario_id, input$millascanjeadas))
    quary <- 0
    quary
  }
  quary
  
}
