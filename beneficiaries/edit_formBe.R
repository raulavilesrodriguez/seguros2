#-------Edit Form to BENEFICIARY----
edit_formBe <- function(button_id, db, labelMandatory){
  
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
                textInput("nombreBeE", labelMandatory("Nombre"), placeholder = ""),
                selectInput("sexoBeE", labelMandatory("Sexo"), multiple = FALSE, choices = c("", "M", "F"))
              ),
              sliderInput("edadBeE", labelMandatory("Edad"), 0, 100, 1, ticks = TRUE, width = "354px"),
              numericInput("millasrecibidasE", labelMandatory("Millas Recibidas"), value = "", min = 0),
              textInput("cedulaBeE", labelMandatory("Cédula"), placeholder = ""),
              textInput("emailBeE", labelMandatory("Email"), placeholder = ""),
              selectInput("estadoBeE", labelMandatory("Estado"), multiple = FALSE, choices = c("", "ACTIVO", "INACTIVO"), width = "354px"),
              helpText(labelMandatory(""), paste("Campo Obligatorio")),
              actionButton(button_id, "Aceptar")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}