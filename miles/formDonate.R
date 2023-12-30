formDonate <- function(button_id, db, input, labelMandatory){
  SQL_clientes <- dbReadTable(db, "responses_df")
  SQL_beneficiarios <- dbReadTable(db, "beneficiario_df")
  showModal(
    modalDialog(
      div(
          tags$head(tags$style(".modal-dialog{ width:400px}")), #Modify the width of the dialog
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), #Necessary to show the input options
          fluidPage(
            fluidRow(
              splitLayout(
                cellWidths = c("250px", "100px"),
                cellArgs = list(style = "vertical-align: top"),
                selectInput("cliente", labelMandatory("Cliente"), choices = c("",SQL_clientes[,c("nombre")]), multiple = FALSE),
              ),
              span(textOutput("cedulaCliente"), style="font-weight:bold;
                       font-family:sans-serif; color:#0F2167"),
              span(textOutput("valmax"), style="font-weight:bold;
                       font-family:sans-serif; color:#750E21"),
              HTML(r"(<br>)"),
              splitLayout(
                cellWidths = c("250px", "100px"),
                cellArgs = list(style = "vertical-align: top"),
                selectInput("beneficiario", labelMandatory("Beneficiario"), choices = c("",SQL_beneficiarios[,c("nombre")]), multiple = FALSE),
              ),
              span(textOutput("cedulaBeneficiario"), style="font-weight:bold;
                       font-family:sans-serif; color:#0F2167"),
              span(textOutput("millasBeneficiario"), style="font-weight:bold;
                       font-family:sans-serif; color:#750E21"),
              HTML(r"(<br>)"),
              numericInput("millasrecibidas", labelMandatory("Millas a Recibir Beneficiario"), 
                           value = "", min = 0, max = 100, width = "354px"),
              
              
              helpText(labelMandatory(""), paste("Campo Obligatorio")),
              actionButton(button_id, "Aceptar")
            ),
            
            
          )
      ),
      title = "Donación Millas",
      easyClose = TRUE,
      footer = modalButton("Cerrar")
    )
  )
}