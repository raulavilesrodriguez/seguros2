#______Entry form CLIENT__________
entry_form <- function(button_id, db, labelMandatory){
  SQL_planes <- dbReadTable(db, "planes")
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
                textInput("nombre", labelMandatory("Nombre"), placeholder = ""),
                selectInput("sexo", labelMandatory("Sexo"), multiple = FALSE, choices = c("", "M", "F"))
              ),
              sliderInput("edad", labelMandatory("Edad"), 0, 100, 1, ticks = TRUE, width = "354px"),
              numericInput("millas", labelMandatory("Millas"), value = "", min = 0),
              textInput("cedula", labelMandatory("Cédula"), placeholder = ""),
              textInput("email", labelMandatory("Email"), placeholder = ""),
              checkboxGroupInput("planes", labelMandatory('Planes'), SQL_planes[,"nombre_plan"], selected = NULL, inline = TRUE),
              textAreaInput("comentario", "Comentario", placeholder = "", height = 100, width = "354px"),
              helpText(labelMandatory(""), paste("Campo Obligatorio")),
              actionButton(button_id, "Aceptar")
            ),
            easyClose = TRUE
          )
      )
    )
  )
}