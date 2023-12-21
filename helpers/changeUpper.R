changeUpper <- function(data){
  x <- c(str_to_upper(str_split(data, regex("\\s"), simplify = TRUE)))
  result <- ""
  for(i in 1:length(x)){
    if (x[i] != ''){
        result <- paste(result, x[i])
    }
  }
  
  result <- str_squish(result)
  result <- stri_trans_general(result, "Latin-ASCII")
  result
}