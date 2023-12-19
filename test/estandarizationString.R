library(tidyverse)
library(stringr) # to regex
library(purrr)

#str_to_upper(str_split(test1, regex("\\s$"), simplify = TRUE))

x <- c(str_to_upper(str_split(test1, regex("\\s"), simplify = TRUE)))
x
result <- c()
for(i in 1:length(x)){
  if (x[i] != ""){
    result <- paste(result, x[i])
    print(x[i])
  }
}
str_squish(result)








