#---Entry Form to BENEFICIARY----
entry_formBe <- function(button_id, db, labelMandatory){
  
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
                textInput("nombreBe", labelMandatory("Nombre"), placeholder = ""),
                selectInput("sexoBe", labelMandatory("Sexo"), multiple = FALSE, choices = c("", "M", "F"))
              ),
              sliderInput("edadBe", labelMandatory("Edad"), 0, 100, 1, ticks = TRUE, width = "354px"),
              
              textInput("cedulaBe", labelMandatory("Cédula"), placeholder = ""),
              textInput("emailBe", labelMandatory("Email"), placeholder = ""),
              selectInput("estadoBe", labelMandatory("Estado"), multiple = FALSE, choices = c("", "ACTIVO", "INACTIVO"), width = "354px"),
              helpText(labelMandatory(""), paste("Campo Obligatorio")),
              actionButton(button_id, "Aceptar")
            )
          )
      ),
      title = "Ingreso Beneficiarios",
      easyClose = TRUE,
      footer = modalButton("Cerrar")
    )
  )
}