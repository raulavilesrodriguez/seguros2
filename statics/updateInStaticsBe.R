updateInStaticsBe <- function(db, input, session){
  observeEvent(input$submitDonate, priority = 20, {
                   
                   x <- dbReadTable(db, "beneficiario_df")
                   freezeReactiveValue(input, 'staticsBe')
                   updateSelectInput(session, "staticsBe", choices = x[,"nombre"])
  })
  
  observeEvent(input$submitCanje, priority = 20, {
    
    x <- dbReadTable(db, "beneficiario_df")
    freezeReactiveValue(input, 'staticsBe')
    updateSelectInput(session, "staticsBe", choices = x[,"nombre"])
  })
  
  observeEvent(input$submit, priority = 20, {
    
    x <- dbReadTable(db, "beneficiario_df")
    freezeReactiveValue(input, 'staticsBe')
    updateSelectInput(session, "staticsBe", choices = x[,"nombre"])
  })
  
  observeEvent(input$submit_edit, priority = 20, {
    
    x <- dbReadTable(db, "beneficiario_df")
    freezeReactiveValue(input, 'staticsBe')
    updateSelectInput(session, "staticsBe", choices = x[,"nombre"])
  })
  
  observeEvent(input$delete_button, priority = 20, {
    
    x <- dbReadTable(db, "beneficiario_df")
    freezeReactiveValue(input, 'staticsBe')
    updateSelectInput(session, "staticsBe", choices = x[,"nombre"])
  })
  
  observeEvent(input$yes_button, priority = 20, {
    
    x <- dbReadTable(db, "beneficiario_df")
    freezeReactiveValue(input, 'staticsBe')
    updateSelectInput(session, "staticsBe", choices = x[,"nombre"])
  })
  
  observeEvent(input$no_button, priority = 20, {
    
    x <- dbReadTable(db, "beneficiario_df")
    freezeReactiveValue(input, 'staticsBe')
    updateSelectInput(session, "staticsBe", choices = x[,"nombre"])
  })
  
  observeEvent(input$submitBe, priority = 20, {
    
    x <- dbReadTable(db, "beneficiario_df")
    freezeReactiveValue(input, 'staticsBe')
    updateSelectInput(session, "staticsBe", choices = x[,"nombre"])
  })
  
  observeEvent(input$submit_edit_Be, priority = 20, {
    
    x <- dbReadTable(db, "beneficiario_df")
    freezeReactiveValue(input, 'staticsBe')
    updateSelectInput(session, "staticsBe", choices = x[,"nombre"])
  })
  
  
  observeEvent(input$delete_beneficiario, priority = 20, {
    
    x <- dbReadTable(db, "beneficiario_df")
    freezeReactiveValue(input, 'staticsBe')
    updateSelectInput(session, "staticsBe", choices = x[,"nombre"])
  })
  
  observeEvent(input$yes_buttonBe, priority = 20, {
    
    x <- dbReadTable(db, "beneficiario_df")
    freezeReactiveValue(input, 'staticsBe')
    updateSelectInput(session, "staticsBe", choices = x[,"nombre"])
  })
  
  observeEvent(input$no_buttonBe, priority = 20, {
    
    x <- dbReadTable(db, "beneficiario_df")
    freezeReactiveValue(input, 'staticsBe')
    updateSelectInput(session, "staticsBe", choices = x[,"nombre"])
  })
  
  
}













