dataBoxStaticsBe <- function(db, input, output, session){
  edadBeneficiario <- dbGetQuery(db, sprintf("SELECT edad 
                                     FROM beneficiario_df 
                                    WHERE nombre = '%s';", input$staticsBe))
  emailBeneficiario <- dbGetQuery(db, sprintf("SELECT email 
                                     FROM beneficiario_df 
                                    WHERE nombre = '%s';", input$staticsBe))
  cedulaBeneficiario <- dbGetQuery(db, sprintf("SELECT cedula 
                                     FROM beneficiario_df 
                                    WHERE nombre = '%s';", input$staticsBe))
  millasBeneficiario <- dbGetQuery(db, sprintf("SELECT millasrecibidas 
                                     FROM beneficiario_df 
                                    WHERE nombre = '%s';", input$staticsBe))
  beneficiario_id <- dbGetQuery(db, sprintf("SELECT beneficiario_id 
                                     FROM beneficiario_df 
                                    WHERE nombre = '%s';", input$staticsBe))
  
  dinersBeneficiario <- dbGetQuery(db, sprintf("SELECT millascanjeadas
                                      FROM diners
                                      WHERE beneficiario_id = '%s';",
                                               beneficiario_id))
  print(sum(dinersBeneficiario[,1]))
  transxDiners <- nrow(dinersBeneficiario)
  
  SQL_Clientes <- dbGetQuery(db, sprintf("SELECT
                                        r.nombre,
                                        c.millas
                                      FROM
                                        benefcliente c
                                      INNER JOIN responses_df r
                                      	ON r.row_id = c.row_id
                                      WHERE c.beneficiario_id = '%s';", 
                                    beneficiario_id))
  
  num_clientes <- SQL_Clientes |> group_by(nombre) |>
                  summarize(num_of_nombres = n())
  
  clientesNombresJoin <- paste(c(num_clientes[['nombre']]), collapse = ", ")
  print(clientesNombresJoin)
  
  transxClientes <- nrow(SQL_Clientes)
  
  output$edadBeStat1 <- renderValueBox({
    valueBox(
      edadBeneficiario, "Edad", icon = icon("person-arrow-up-from-line"), color = "purple"
    )
  })
  output$emailBeStat1 <- renderValueBox({
    valueBox(
      "Correo", emailBeneficiario, icon = icon("envelopes-bulk"), 
      color = "yellow"
    )
  })
  output$cedulaBeStat1 <- renderValueBox({
    valueBox(
      "Cédula", cedulaBeneficiario, icon = icon("id-card"), 
      color = "orange"
    )
  })
  output$millasBeStat1 <- renderValueBox({
    valueBox(
      millasBeneficiario, "Millas Acumuladas", icon = icon("cart-shopping"), color = "green"
    )
  })
  output$millasCanjeadasBe1 <- renderValueBox({
    valueBox(
      sum(dinersBeneficiario[,1]), "Millas Canjeadas", icon = icon("gift"), color = "maroon"
    )
  })
  output$namesClientes1 <- renderValueBox({
    valueBox(
      "Clientes Asoc", clientesNombresJoin, icon = icon("people-pulling"),
      color = "purple"
    )
  })
  output$transxClientes1 <- renderValueBox({
    valueBox(
      transxClientes, "No. Transacciones Clientes", icon = icon("money-bill-transfer"),
      color = "light-blue"
    )
  })
  output$transxDiners1 <- renderValueBox({
    valueBox(
      transxDiners, "No. Transacciones Club Miles", icon = icon("face-smile-wink"),
      color = "teal"
    )
  })
  
  
  
} 