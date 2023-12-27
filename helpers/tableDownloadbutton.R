### Button functions ###
tableDownloadbutton <- function(outputId, label = NULL){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("download"), label)
}