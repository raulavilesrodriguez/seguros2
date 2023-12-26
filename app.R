library(config)
library(DBI)
library(lubridate)
library(glue)
library(fontawesome)
library(leaflet)
library(shiny)
library(DT)
library(RPostgreSQL)
library(pool)
library(shinyjs)
library(uuid)
library(dplyr)
library(tidyverse)
library(shinydashboard)
library(shinyauthr)
library(readxl)
library(writexl)
library(shinyBS)
library(here)
library(stringr) # to regex
library(stringi) # to remove accents

source(here::here('./tables_sql/tables_SQL.R'))
source(here::here('./helpers/mandatoryFilled.R'))
source(here::here('./helpers/changeUpper.R'))
source(here::here('./helpers/onlyNumbers.R'))

source(here::here('./clients/entry_form.R'))
source(here::here('./clients/appendData.R'))
source(here::here('./clients/appendEdit.R'))
source(here::here('./clients/updateDataEdit.R'))
source(here::here('./clients/deleteDataClient.R'))

source(here::here('./beneficiaries/entry_formBe.R'))
source(here::here('./beneficiaries/appendDataBe.R'))
source(here::here('./beneficiaries/updateBeneficiaries.R'))
source(here::here('./beneficiaries/edit_formBe.R'))
source(here::here('./beneficiaries/appendEditBe.R'))
source(here::here('./beneficiaries/deleteDataBeneficiaries.R'))

source(here::here('./miles/formDonate.R'))
source(here::here('./miles/appendDonate.R'))

#Read the database connection parameters from the config.yml
config_file <- "config.yml"
config <- config::get(file = config_file)

# Create a database connection
db <- dbPool(RPostgres::Postgres(),
                           host = config$host,
                           port = config$port,
                           dbname = config$dbname,
                           user = config$user,
                           password = config$password)

# a user who has not visited the app for this many days
# will be asked to login with user name and password again
cookie_expiry <- 7 # Days until session expires


# This function must accept two parameters: user and sessionid. It will be called whenever the user
# successfully logs in with a password.  This function saves to your database.

add_sessionid_to_db <- function(users, sessionid, conn = db) {
  tibble(users = users, sessionid = sessionid, login_time = as.character(now())) %>%
    dbWriteTable(conn, "sessionids", ., append = TRUE)
}

# This function must return a data.frame with columns user and sessionid  Other columns are also okay
# and will be made available to the app after log in as columns in credentials()$user_auth

get_sessionids_from_db <- function(conn = db, expiry = cookie_expiry) {
  dbReadTable(conn, "sessionids") %>%
    mutate(login_time = ymd_hms(login_time)) %>%
    as_tibble() %>%
    filter(login_time > now() - days(expiry))
}


# dataframe that holds usernames, passwords and other user data
user_base <- dbReadTable(db, "acceso") |> as_tibble()

# --------CREATE tables in PostgreSQL-------
responses_df <- tables_SQL(db)



# to mark * any fields in the entry form that are mandatory
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }"

### Button functions ###
tableDownloadbutton <- function(outputId, label = NULL){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, icon("download"), label)
}


#--------Shiny APP---------
##### UI######
ui <- dashboardPage(title = "Millas App", skin= "purple",
  dashboardHeader(
    title = "Millas app",
    tags$li(
      class = "dropdown",
      style = "padding: 8px;",
      shinyauthr::logoutUI("logout") # add logout button UI
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        icon("spaghetti-monster-flying"),
        href = "https://raulaviles.netlify.app/",
        title = "Autor"
      )
    )
  ),
  dashboardSidebar(
    collapsed = TRUE,
    div(icon("circle-user"), HTML("&nbsp;"), textOutput("welcome"), style = "display: flex; align-items: center; padding: 20px"),
    sidebarMenu(
      menuItem("Clientes", tabName = "Clientes", icon = icon("user")),
      menuItem("Beneficiarios", tabName = "Beneficiarios", icon = icon("face-smile")),
      menuItem("Asignación", tabName = "Asignación", icon = icon("wand-magic-sparkles"))
    )
  ),
  dashboardBody(
    includeCSS("www/custom.css"),
    tags$head(
      tags$style(
        HTML("
      @media only screen and (max-width: 600px) {
        .responsive-table {
          width: 100% !important;
          font-size:80%;
        }
      }
      
      @media only screen and (min-width: 601px) {
        .responsive-table {
          width: 1000px !important;
          font-size:80%;
        }
      }
    ")
      ),
      tags$script(HTML(
        "$(document).on('shown.bs.modal','#shiny-modal', function () {
       Shiny.setInputValue(id = 'modal_visible', value = true);
      });
     $(document).on('hidden.bs.modal','#shiny-modal', function () {
       Shiny.setInputValue(id = 'modal_visible', value = false);
     });"
      )),
    ),
    tags$style(HTML("
                    .content { padding: 50px; }
                    
                    .download-container{
                      text-align: right;
                      padding-right: 10px;
                      top: 0px;
                      height: 20px;
                    }
                    
                    ")),
    
    shinyauthr::loginUI(
      id = "login", 
      cookie_expiry = cookie_expiry,
      title = "Ingreso Seguro App"
    ),
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    tags$div(id = "tabs",
      tabItems(
        tabItem(tabName = "Clientes",
                fluidRow(
                  actionButton("add_button", "Añadir", icon("plus")),
                  actionButton("edit_button", "Editar", icon("edit")),
                  actionButton("delete_button", "Eliminar", icon("trash-alt")),
                  
                ),
                fluidRow(
                  uiOutput("download"),
                ),
                br(),
                fluidRow(width="100%",
                         DT::dataTableOutput("responses_table")
                )
        ),
        tabItem(tabName = "Beneficiarios",
                fluidRow(
                  actionButton("add_beneficiario", "Añadir", icon("plus")),
                  actionButton("edit_beneficiario", "Editar", icon("edit")),
                  actionButton("delete_beneficiario", "Eliminar", icon("trash-alt")),
                ),
                fluidRow(
                  uiOutput("downloadBe"),
                ),
                br(),
                fluidRow(width="100%",
                         DT::dataTableOutput("table_beneficiaries")
                )
                
        ),
        tabItem(tabName = "Asignación",
                fluidRow(
                  actionButton("donate", "Asignar", icon("bolt"))
                )
                
                )
        
      )
    )
    
  )
)

server <- function(input, output, session) {
  
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  # call login module supplying data frame, user and password cols
  # and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = users,
    pwd_col = password,
    cookie_logins = TRUE,
    sessionid_col = sessionid,
    cookie_getter = get_sessionids_from_db,
    cookie_setter = add_sessionid_to_db,
    log_out = reactive(logout_init())
  )
  
  observe({
    if (credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      shinyjs::show("tabs")
      shinyjs::show(selector = ".main-header")
      
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      shinyjs::hide("tabs")
      shinyjs::hide(selector = ".main-header")
    }
  })
  
  user_info <- reactive({
    credentials()$info
  })
  
  output$welcome <- renderText({
    req(credentials()$user_auth)
    
    glue("Bienvenido {user_info()$name}")
  })
  
  # Enter the inputs to make the df CLIENTS reactive to any input changes.
  responses_df <- reactive({
    
    input$submit
    input$submit_edit
    input$delete_button
    input$yes_button
    
    dbReadTable(db, "responses_df")
  })
  
  # Enter the inputs to make the df BENEFICIARIES reactive to any input changes.
  beneficiario_df <- reactive({
    input$submitBe
    input$submit_edit_Be
    input$delete_beneficiario
    input$yes_buttonBe
    
    dbReadTable(db, "beneficiario_df")
  })
  
  
  
  # Enter the name of the fields that should be mandatory to fill out CLIENTES
  fieldsMandatoryClient <- c("nombre", "sexo", "edad", "millas", "cedula", "email", "planes")
  
  # Function to observe if all mandatory fields are filled out. 
  #If TRUE the submit button will become activated
  mandatoryFilled("submit", fieldsMandatoryClient, input)
  mandatoryFilled("submit_edit", fieldsMandatoryClient, input)
  
  # Enter the name of the fields that should be mandatory to fill out BENEFICIARIES
  fieldsMandatoryBe <- c("nombreBe", "sexoBe", "edadBe", "cedulaBe", "emailBe", "estadoBe")
  mandatoryFilled("submitBe", fieldsMandatoryBe, input)
  fieldsMandatoryBeE <- c("nombreBeE", "sexoBeE", "edadBeE", "cedulaBeE", "emailBeE", "estadoBeE")
  mandatoryFilled("submit_edit_Be", fieldsMandatoryBeE, input)
  
  fieldsMandatoryDonate <- c("cliente", "beneficiario", "millasrecibidas")
  mandatoryFilled("submitDonate", fieldsMandatoryDonate, input)
  
  #____Add Data_____
  # Function to save the data into df format
  formData <- reactive({
    # Change to UPPER the name variable and eliminate Spaces
    newName <- changeUpper(input$nombre)
    newCedula <- onlyNumbers(input$cedula)
    
    formData <- data.frame(row_id = UUIDgenerate(),
                           nombre = newName,
                           sexo = input$sexo,
                           edad = input$edad,
                           millas = input$millas,
                           cedula = newCedula,
                           email = input$email,
                           comentario = input$comentario,
                           creado = as.character(format(Sys.Date(), format="%Y-%m-%d")),
                           stringsAsFactors = FALSE)
    return(formData)
  })
  
  
  # When add button is clicked it will activate the entry_form with an 
  #action button called submit CLIENT
  observeEvent(input$add_button, priority = 20,{
    entry_form("submit", db, labelMandatory)
    
  })
  
  
  # reset and the modal is removed
  observeEvent(input$submit, priority = 20,{
    
    quary <- appendData(formData(), db, input)
    showModal(
      if(quary == 1){
        modalDialog(
          title = "Advertencia",
          paste("No pueden haber 2 clientes con el mismo nombre o la misma cédula" ),easyClose = TRUE
        ) 
      }
    )
    if(quary == 0){
      shinyjs::reset("entry_form")
      removeModal() 
    }
  })
  
  
  #________ADD Data BENEFICIARIES________
  formDataBe <- reactive({
    
    newNameBe <- changeUpper(input$nombreBe)
    newCedulaBe <- onlyNumbers(input$cedulaBe)
    
    formDataBe <- data.frame(beneficiario_id = UUIDgenerate(),
                           nombre = newNameBe,
                           sexo = input$sexoBe,
                           edad = input$edadBe,
                           cedula = newCedulaBe,
                           email = input$emailBe,
                           estado = input$estadoBe,
                           stringsAsFactors = FALSE)
    return(formDataBe)
  })
  
  
  observeEvent(input$add_beneficiario, priority = 20, {
    entry_formBe("submitBe", db, labelMandatory)
  })
  
  observeEvent(input$submitBe, priority = 20, {
    quary <- appendDataBe(formDataBe(), db, input)
    showModal(
      if(quary == 1){
        modalDialog(
          title = "Advertencia",
          paste("No pueden haber 2 Beneficiarios con la misma cédula" ),easyClose = TRUE
        ) 
      }
    )
    if(quary == 0){
      shinyjs::reset("entry_formBe")
      removeModal() 
    }
    
  })
  
  #___Delete Data CLIENT____
  deleteDataClient(db, input)
  
  #___Delete Data BENEFICIARIES____
  deleteDataBeneficiaries(db, input)
  
  
  # ___________Edit DATA CLIENT_______________
  observeEvent(input$edit_button, priority = 20,{
    updateDataEdit(db, input, session, labelMandatory)
  })
  
  # Update the selected row with the values that were entered in the form
  observeEvent(input$submit_edit, priority = 20, {
    appendEdit(db, input)
  })
  
  
  #____________Edit DATA BENEFICIARIES__________
  observeEvent(input$edit_beneficiario, priority = 20, {
    updateBeneficiaries(db, input, session, labelMandatory)
  })
  
  
  observeEvent(input$submit_edit_Be, priority = 20, {
    appendEditBe(db, input)
  })
  
  
  #__________ASSIGN MILLES FROM CLIENT TO BENEFICIARIES_____________
  observeEvent(input$donate, priority = 20, {
    formDonate("submitDonate", db, input, labelMandatory)
  })
  
  # update data of Client cedula and miles in the Form
  observeEvent(input$cliente, priority = 20, {
    if(input$cliente !=""){
      cedulaCliente <- dbGetQuery(db, sprintf("SELECT cedula FROM responses_df WHERE nombre = '%s';", input$cliente))
      print(cedulaCliente[1,1])
      output$cedulaCliente <- renderText({
        paste("Cédula Cliente: ", cedulaCliente[1,1])
      })
      valmax <- dbGetQuery(db, sprintf("SELECT millas FROM responses_df WHERE nombre = '%s';", input$cliente))
      print(valmax[1,1])
      output$valmax <- renderText({
        paste("Millas Cliente: ", valmax[1,1])
      })
      updateSliderInput(session, "millasrecibidas", value = as.numeric(valmax[1,1]), min = 0, max = as.numeric(valmax[1,1]))
    }
  })
  
  # update data of beneficiarie cedula in the Form
  observeEvent(input$beneficiario, priority = 20, {
    if(input$beneficiario != ""){
      cedulaBeneficiario <- dbGetQuery(db, sprintf("SELECT cedula FROM beneficiario_df WHERE nombre = '%s';", input$beneficiario))
      print(cedulaBeneficiario[1,1])
      output$cedulaBeneficiario <- renderText({
        paste("Cédula Beneficiario: ", cedulaBeneficiario[1,1])
      })
      millasBeneficiario <- dbGetQuery(db, sprintf("SELECT millasrecibidas FROM beneficiario_df WHERE nombre = '%s';", input$beneficiario))
      print(millasBeneficiario[1,1])
      output$millasBeneficiario <- renderText({
        paste("Millas Beneficiario: ", millasBeneficiario[1,1])
      })
    } 
  })
  
  observeEvent(input$submitDonate, priority = 20, {
    quaryDonate <- appendDonate(db, input)
    showModal(
      if(quaryDonate == 1){
        modalDialog(
          title = "Advertencia",
          paste("Millas insuficientes del Cliente. Acción no ejecutada."),easyClose = TRUE,
          footer = modalButton("Cerrar")
        ) 
      }
    )
    if(quaryDonate == 0){
      shinyjs::reset("formDonate")
      removeModal() 
    }
  })
  
  resetOutputs <- function() {
    output$cedulaCliente <- renderText({ "" })
    output$valmax <- renderText({ "" })
    output$cedulaBeneficiario <- renderText({ "" })
    output$millasBeneficiario <- renderText({ "" })
  }
  
  observeEvent({input$modal_visible == FALSE}, priority = 20,{
    resetOutputs()
  }, ignoreInit = TRUE)
  
  #____Download button_____
  # Download function DataBase
  download_df <- reactive({
    download_df <- dbReadTable(db, "responses_df")
    return(download_df)
  })
  
  output$download_button <- downloadHandler(
    filename = function() {"respaldo.xlsx"},
    content = function(file){writexl::write_xlsx(download_df(), file)
    })
  
  output$download <- renderUI({
    div(class = "download-container",
        tableDownloadbutton("download_button", label=NULL),
        bsTooltip(id = "download_button", title = "Descarga", 
                  placement = "left", trigger = "hover")
        )
  })
  
  
  
  #________Displaying the Data Table CLIENTS_____________
  output$responses_table <- DT::renderDataTable({
    table <- responses_df() %>% select(-c(row_id, beneficiario_id)) 
    names(table) <- c(
      "Nombre", "Sexo", "Edad", "Millas", "Cédula", "Email", "Commentario", "Creado")
    table <- datatable(table, 
                       rownames = FALSE,
                       options = list(
                         searching = TRUE, 
                         lengthChange = TRUE,
                         bSortClasses = TRUE,iDisplayLength = 10,   width = "100%",
                         scrollX=TRUE,
                         autoWidth = TRUE,
                         columnDefs = list(
                           list(width = '100px', targets = "_all") # Adjust the width as needed
                         )
                         )
    )
  })
  
  #________Displaying the Data Table BENEFICIARIES_____________
  output$table_beneficiaries <- DT::renderDataTable({
    table <- beneficiario_df() |> select(-c(beneficiario_id))
    names(table) <- c(
      "Nombre", "Sexo", "Edad", "Millas Recibidas","Cédula", "Email", "Estado", "Creado"
    )
    table <- datatable(table,
                       rownames = FALSE,
                       options = list(
                         searching = TRUE, 
                         lengthChange = TRUE,
                         bSortClasses = TRUE,iDisplayLength = 10,   width = "100%",
                         scrollX=TRUE,
                         autoWidth = TRUE,
                         columnDefs = list(
                           list(width = '100px', targets = "_all") # Adjust the width as needed
                         )
                       ))
    
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)









