formCanje <- function(button_id, db, input, labelMandatory){
  
  SQL_beneficiarios <- dbReadTable(db, "beneficiario_df")
  showModal(
    modalDialog(
      div(id=("canje_form"),
          tags$head(tags$style(".modal-dialog{ width:400px}")), #Modify the width of the dialog
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), #Necessary to show the input options
          fluidPage(
            fluidRow(
              splitLayout(
                cellWidths = c("250px", "100px"),
                cellArgs = list(style = "vertical-align: top"),
                selectInput("beneficiarioCj", labelMandatory("Beneficiario"), choices = c("",SQL_beneficiarios[,c("nombre")]), multiple = FALSE),
              ),
              span(textOutput("cedulaBeneficiarioCj"), style="font-weight:bold;
                       font-family:sans-serif; color:#0F2167"),
              span(textOutput("millasBeneficiarioCj"), style="font-weight:bold;
                       font-family:sans-serif; color:#750E21"),
              HTML(r"(<br>)"),
              numericInput("millascanjeadas", labelMandatory("Millas a Canjear con Club Miles"), 
                           value = "", min = 0, max = 100, width = "354px"),
              
              
              helpText(labelMandatory(""), paste("Campo Obligatorio")),
              actionButton(button_id, "Aceptar")
            ),
            
            
          )
      ),
      title = "Canje de Millas",
      easyClose = TRUE,
      footer = modalButton("Cerrar")
    )
  )
}
