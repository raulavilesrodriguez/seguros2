onlyNumbers <- function(data){
  cleaned_data <- str_extract_all(data, regex("[:digit:]"), simplify = TRUE)
  finalCedula <- c()
  for(i in 1: length(cleaned_data)){
    finalCedula <- paste0(finalCedula, cleaned_data[i])
  }
  finalCedula
}