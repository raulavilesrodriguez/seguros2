# Function to append data to the SQL table
appendDataBe <- function(data, db, input){
  
  cedula_unica <- dbGetQuery(db, sprintf(
                              "SELECT * FROM beneficiario_df
                              WHERE cedula = '%s';", data[['cedula']]
                            ))
  
  nombre_unico <- dbGetQuery(db, 
                             sprintf(
                               "SELECT * FROM beneficiario_df
                                   WHERE nombre = '%s';", data[['nombre']] 
                             ))
  
  quary <- 1
  if(nrow(cedula_unica) == 0 & nrow(nombre_unico) == 0){
    dbWriteTable(db, "beneficiario_df", data, append = TRUE)
    
    quary <- 0
    quary
  }
  quary
}
