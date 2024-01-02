entry_formPlans <- function(button_id, db, labelMandatory){
  showModal(
    modalDialog(
      div(id=("entry_form"),
          tags$head(tags$style(".modal-dialog{ width:400px}")), #Modify the width of the dialog
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), #Necessary to show the input options
          fluidPage(
            fluidRow(
              splitLayout(
                cellWidths = c("250px", "100px"),
                cellArgs = list(style = "vertical-align: top"),
                textInput("nombrePlan", labelMandatory("Nombre Plan"), placeholder = "")
              ),
              helpText(labelMandatory(""), paste("Campo Obligatorio")),
              actionButton(button_id, "Aceptar")
            )
          )
      ),
      title = "Ingreso Planes",
      easyClose = TRUE,
      footer = modalButton("Cerrar")
    )
  )
}