# Function to observe if all mandatory fields are filled out. 
#If TRUE the submit button will become activated

mandatoryFilled <- function(boton, fieldsMandatory, input){
  observe({
    
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]][1]) && input[[x]][1] != ""
             },
             logical(1))
    
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = boton, 
                         condition = mandatoryFilled)
  })
}