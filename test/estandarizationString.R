library(tidyverse)
library(stringr) # to regex
library(purrr)
library(stringi)

#str_to_upper(str_split(test1, regex("\\s$"), simplify = TRUE))
test0 <- "           LulA   MoREno   zARaTe"
test1 <- "  Raúl    Avilés   "

x <- c(str_to_upper(str_split(test1, regex("\\s"), simplify = TRUE)))
x
result <- c()
for(i in 1:length(x)){
  if (x[i] != ""){
    result <- paste(result, x[i])
    print(x[i])
  }
}
result <- str_squish(result)
result <- stri_trans_general(result, "Latin-ASCII")
result



test4 <- "*123456789-0  ú  // {}\\ saPo 1"
cleaned_test4 <- str_extract_all(test4, regex("[:digit:]"), simplify = TRUE)
finalCedula <- c()
for(i in 1: length(cleaned_test4)){
  finalCedula <- paste0(finalCedula, cleaned_test4[i])
}
finalCedula


