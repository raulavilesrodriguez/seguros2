# Function to append data to the SQL table
appendData <- function(data, db, input){
  
  cedula_unica <- dbGetQuery(db, 
                             sprintf(
                               "SELECT * FROM responses_df
                                WHERE cedula = '%s';", data[['cedula']]
                             ))
  
  nombre_unico <- dbGetQuery(db, 
                              sprintf(
                                  "SELECT * FROM responses_df
                                   WHERE nombre = '%s';", data[['nombre']] 
                              ))
  
  quary <- 1
  if(nrow(cedula_unica) == 0 & nrow(nombre_unico) == 0){
    dbWriteTable(db, "responses_df", data, append = TRUE)
    sapply(input$planes, function(p){
      if(!is.null(p)){
        planes_id <- dbGetQuery(db, 
                                sprintf(
                                  "SELECT planes_id FROM planes 
                                     WHERE nombre_plan = '%s';", p
                                ))
        
        dbExecute(db, sprintf("INSERT INTO
                                  clientePlanes (row_id, planes_id)
                                VALUES ('%s', '%s');", data[['row_id']], planes_id)) 
      }
    })      
    quary <- 0
    quary
  }
  quary
}
