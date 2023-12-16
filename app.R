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

source(here::here('./tables_sql/tables_SQL.R'))
source(here::here('./helpers/mandatoryFilled.R'))
source(here::here('./clients/entry_form.R'))
source(here::here('./beneficiaries/entry_formBe.R'))

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
ui <- dashboardPage(title = "Millas App",
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
      menuItem("Clientes", tabName = "Clientes", icon = icon("dashboard")),
      menuItem("Beneficiarios", tabName = "Beneficiarios", icon = icon("th"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css"),
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
      )
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
  
  # Enter the inputs to make the df reactive to any input changes.
  responses_df <- reactive({
    
    input$submit
    input$submit_edit
    input$delete_button
    input$yes_button
    
    dbReadTable(db, "responses_df")
  })
  
  # Enter the name of the fields that should be mandatory to fill out
  fieldsMandatoryClient <- c("nombre", "sexo", "edad", "millas", "cedula", "email", "planes")
  
  # Function to observe if all mandatory fields are filled out. 
  #If TRUE the submit button will become activated
  mandatoryFilled("submit", fieldsMandatoryClient, input)
  mandatoryFilled("submit_edit", fieldsMandatoryClient, input)
  
  
  
  #____Add Data_____
  # Function to save the data into df format
  formData <- reactive({
    
    formData <- data.frame(row_id = UUIDgenerate(),
                           nombre = input$nombre,
                           sexo = input$sexo,
                           edad = input$edad,
                           millas = input$millas,
                           cedula = input$cedula,
                           email = input$email,
                           comentario = input$comentario,
                           creado = as.character(format(Sys.Date(), format="%Y-%m-%d")),
                           stringsAsFactors = FALSE)
    return(formData)
  })
  
  # Function to append data to the SQL table
  appendData <- function(data){
    consulting <- glue("SELECT * FROM responses_df
                        WHERE cedula = '{input$cedula}';")
    cedula_unica <- dbGetQuery(db, consulting)
    quary <- 1
    if(nrow(cedula_unica) == 0){
       dbWriteTable(db, "responses_df", data, append = TRUE)
       sapply(input$planes, function(p){
         if(!is.null(p)){
           planes_id <- dbGetQuery(db, 
                                   sprintf(
                                     "SELECT planes_id FROM planes 
                                     WHERE nombre_plan = '%s';", p
                                   ))
           
           dbExecute(db, sprintf("INSERT INTO
                                  clientePlanes (row_id, planes_id)
                                VALUES ('%s', '%s');", data[['row_id']], planes_id)) 
         }
       })      
       quary <- 0
       quary
    }
    quary
    }
    
  
  # When add button is clicked it will activate the entry_form with an 
  #action button called submit CLIENT
  observeEvent(input$add_button, priority = 20,{
    entry_form("submit", db, labelMandatory)
    
  })
  
  
  # reset and the modal is removed
  observeEvent(input$submit, priority = 20,{
    
    quary <- appendData(formData())
    showModal(
      if(quary == 1){
        modalDialog(
          title = "Advertencia",
          paste("No pueden haber 2 clientes con la misma cédula" ),easyClose = TRUE
        ) 
      }
    )
    if(quary == 0){
      shinyjs::reset("entry_form")
      removeModal() 
    }
  })
  
  
  # ----To update data in Beneficiaries Modal---
  observeEvent(input$cliente, priority = 20, {
    if(input$cliente !=""){
      valmax <- dbGetQuery(db, sprintf("SELECT millas FROM responses_df WHERE nombre = '%s';", input$cliente))
      print(valmax[1,1])
      updateSliderInput(session, "millasRecibidas", value = 0, min = 0, max = as.numeric(valmax[1,1]))
    }
  })
  
  
  observeEvent(input$add_beneficiario, priority = 20, {
    entry_formBe("submitBe", db, labelMandatory)
  })
  
  #___Delete Data CLIENT____
  row_selection <- reactiveValues(
    # This will return an empty data frame
    rows = data.frame()
  )
  
  deleteData <- reactive({
    quary <- lapply(row_selection$rows, function(nr){
      dbExecute(db, sprintf("DELETE FROM clientePlanes WHERE row_id = '%s';", nr))
      dbExecute(db, sprintf("DELETE FROM responses_df WHERE row_id = '%s'", nr))
    })
  })
  
  # Delete rows when selected. Otherwise display error message
  observeEvent(input$delete_button, priority = 20,{
    
    if(length(input$responses_table_rows_selected)>=1 ){
      SQL_df <- dbReadTable(db, "responses_df")
      row_selection$rows <- SQL_df[input$responses_table_rows_selected, "row_id"]
      showModal(
        modalDialog(id="delete_modal",
                    title = "Advertencia",
                    paste("Estás seguro de borrar el/los cliente(s)?"),
                    br(),
                    br(),
                    actionButton("yes_button", "Si"),
                    actionButton("no_button", "No"),
                    easyClose = TRUE, footer = NULL))
      
    }
    
    else if(length(input$responses_table_rows_selected) < 1 ){
      showModal(
        modalDialog(
          title = "Advertencia",
          paste("Por favor selecciona el/los cliente(s)" ),easyClose = TRUE
        ) 
      )
    }
    
  })
  
  observeEvent(input$yes_button, priority = 20,{
    
    deleteData()
    removeModal()
    
  })
  
  observeEvent(input$no_button, priority = 20,{
    removeModal()
    
  })
  
  # ___________Edit DATA CLIENT_______________
  observeEvent(input$edit_button, priority = 20,{
    
    SQL_df <- dbReadTable(db, "responses_df")
    
    
    showModal(
      if(length(input$responses_table_rows_selected) > 1 ){
        modalDialog(
          title = "Advertencia",
          paste("Por favor selecciona solo un cliente" ),easyClose = TRUE)
      } else if(length(input$responses_table_rows_selected) < 1){
        modalDialog(
          title = "Advertencia",
          paste("Por favor selecciona un cliente" ),easyClose = TRUE)
      })  
    
    if(length(input$responses_table_rows_selected) == 1 ){
      
      entry_form("submit_edit", db, labelMandatory)
      
      # to select planes of an client
      row_selection <- SQL_df[input$responses_table_rows_selected, "row_id"]
      SQL_planes <- dbReadTable(db, "planes")
      planes_selected <- dbGetQuery(db, 
                              sprintf(
                                "SELECT 
                                  	p.nombre_plan
                                  FROM 
                                  	planes p
                                  INNER JOIN clienteplanes c
                                  	ON p.planes_id = c.planes_id
                                  WHERE c.row_id = '%s';", 
                                row_selection
                              ))
      
      banana <- c(planes_selected[['nombre_plan']])
      updateTextInput(session, "nombre", value = SQL_df[input$responses_table_rows_selected, "nombre"])
      updateSelectInput(session, "sexo", selected = SQL_df[input$responses_table_rows_selected, "sexo"])
      updateSliderInput(session, "edad", value = SQL_df[input$responses_table_rows_selected, "edad"])
      updateNumericInput(session, "millas", value = SQL_df[input$responses_table_rows_selected, "millas"])
      updateTextInput(session, "cedula", value = SQL_df[input$responses_table_rows_selected, "cedula"])
      updateTextInput(session, "email", value = SQL_df[input$responses_table_rows_selected, "email"])
      updateCheckboxGroupInput(session, "planes", 'Planes', SQL_planes[,"nombre_plan"], inline = TRUE, selected = banana)
      updateTextAreaInput(session, "comentario", value = SQL_df[input$responses_table_rows_selected, "comentario"])
      
    }
    
  })
  
  # Update the selected row with the values that were entered in the form
  observeEvent(input$submit_edit, priority = 20, {
    
    SQL_df <- dbReadTable(db, "responses_df")
    row_selection <- SQL_df[input$responses_table_row_last_clicked, "row_id"]
    
    
    consulting <- glue("SELECT * FROM responses_df
                        WHERE cedula = '{input$cedula}' AND
                        nombre != '{input$nombre}';")
    cedula_unica <- dbGetQuery(db, consulting)
    print(nrow(cedula_unica))
    if(nrow(cedula_unica) == 0){
      # CHANGE IN IF to not duplicate the planes chosen
      dbExecute(db, sprintf("DELETE FROM clientePlanes
                           WHERE row_id = '%s';", row_selection))
      
      sapply(input$planes, function(p){
        if(!is.null(p)){
          planes_id <- dbGetQuery(db, 
                                  sprintf(
                                    "SELECT planes_id FROM planes 
                                     WHERE nombre_plan = '%s';", p
                                  ))
          
          dbExecute(db, sprintf("INSERT INTO
                                  clientePlanes (row_id, planes_id)
                                VALUES ('%s', '%s');", row_selection, planes_id)) 
        }
      })
      
      dbExecute(db, sprintf("UPDATE responses_df SET nombre = $1, sexo = $2, 
                          edad = $3, millas = $4, cedula = $5, email = $6,
                          comentario = $7 WHERE row_id = '%s'", row_selection), 
                param = list(input$nombre,
                             input$sexo,
                             input$edad,
                             input$millas,
                             input$cedula,
                             input$email,
                             input$comentario))
      removeModal()
    } else {
       showModal(
         modalDialog(
           title = "Advertencia. Cambios no grabados",
           paste("No pueden haber 2 clientes con la misma cédula" ),easyClose = TRUE
         )
       )
    }
    
  })
  
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
  
  
  
  #________Displaying the Data Table_____________
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
}

shinyApp(ui = ui, server = server)









