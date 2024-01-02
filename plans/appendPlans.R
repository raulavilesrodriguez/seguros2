appendPlans <- function(data, db, input){
  nombre_unico <- dbGetQuery(db, 
                             sprintf(
                               "SELECT * FROM planes
                                   WHERE nombre_plan = '%s';", data[['nombre_plan']] 
                             ))
  
  quary <- 1
  if(nrow(nombre_unico) == 0){
    dbWriteTable(db, "planes", data, append = TRUE)
    
    quary <- 0
    quary
  }
  quary
}