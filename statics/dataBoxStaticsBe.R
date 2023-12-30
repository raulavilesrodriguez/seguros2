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
  
  rows_id <- dbGetQuery(db, sprintf("SELECT row_id
                                      FROM benefcliente
                                      WHERE beneficiario_id = '%s';", 
                                    beneficiario_id))
  
  
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
  
  
  
  
  
} 