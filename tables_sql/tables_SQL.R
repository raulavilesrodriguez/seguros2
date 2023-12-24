tables_SQL <- function(db){
  # create beneficiario table
  result <- try({
    dbGetQuery(db, 
               "SELECT * FROM beneficiario_df;")  
  }, silent = TRUE)
  if(inherits(result, 'try-error')){
    dbGetQuery(db, 
               "CREATE TABLE beneficiario_df (
	                beneficiario_id VARCHAR PRIMARY KEY,
	                nombre VARCHAR (250) NOT NULL,
	                sexo CHAR(1) CHECK (sexo IN ('M', 'F')),
	                edad INT,
	                millasrecibidas INT DEFAULT 0,
	                cedula VARCHAR (255) UNIQUE NOT NULL,
	                email VARCHAR (255) NOT NULL,
	                estado VARCHAR CHECK (estado IN ('ACTIVO', 'INACTIVO')),
	                creado TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
                );")
  }
  
  
  # create the dataframe to 'titular' database. 
  responses_df <- data.frame(row_id = character(),
                             nombre = character(),
                             sexo = character(),
                             edad = as.numeric(character()),
                             millas = numeric(),
                             cedula = character(),
                             email = character(),
                             comentario = character(),
                             creado = as.Date(character()),
                             beneficiario_id = character(),
                             stringsAsFactors = FALSE
  )
  
  # Create responses tabl in PostegreSQL
  dbWriteTable(db, 
               "responses_df",
               responses_df,
               overwrite = FALSE,
               append = TRUE,
  )
  
  # Check if 'row_id' is already a primary key
  existing_primary_key_query  <- dbGetQuery(db, 
                      "SELECT column_name, data_type
                      FROM information_schema.columns
                      WHERE table_name = 'responses_df'
                      AND column_name = 'row_id'
                      AND column_name IN (
                        SELECT column_name
                        FROM information_schema.table_constraints
                        JOIN information_schema.key_column_usage USING (constraint_catalog, constraint_schema, constraint_name, table_catalog, table_schema, table_name)
                        WHERE constraint_type = 'PRIMARY KEY'
                        AND table_name = 'responses_df'
                      );")
  
  
  if (nrow(existing_primary_key_query) == 0) {
    # 'row_id' is not a primary key, so set it as one
    dbExecute(db, "ALTER TABLE responses_df ADD PRIMARY KEY (row_id);")
  }
  
  # Check if 'cedula' is already a unique key
  existing_unique_key_query <- dbGetQuery(db,
                                          "SELECT column_name, data_type
                            FROM information_schema.columns
                            WHERE table_name = 'responses_df'
                            AND column_name = 'cedula'
                            AND column_name IN (
                                SELECT column_name
                                FROM information_schema.table_constraints
                                JOIN information_schema.key_column_usage USING (constraint_catalog, constraint_schema, constraint_name, table_catalog, table_schema, table_name)
                                WHERE constraint_type = 'UNIQUE'
                                AND table_name = 'responses_df'
                            );")
  
  if (nrow(existing_unique_key_query) == 0) {
    # 'cedula' is not a unique key, so set it as one
    dbExecute(db, "ALTER TABLE responses_df ADD CONSTRAINT unique_cedula UNIQUE (cedula);")
  }
  
  
  # Run the SQL query to add a foreign key constraint in responses_df
  try({
    dbExecute(db, 
              "ALTER TABLE responses_df
                ADD CONSTRAINT fk_beneficiario
                FOREIGN KEY (beneficiario_id)
                REFERENCES beneficiario_df(beneficiario_id);"
    )
  }, silent = TRUE)
  
  # Create Planes table
  try({
    dbExecute(db,
              "CREATE TABLE planes(
	              planes_id serial PRIMARY KEY,
	              nombre_plan VARCHAR NOT NULL
                );"
              )
  }, silent = TRUE)
  
  # Create Cliente-Planes table
  try({
    dbExecute(db,
              "CREATE TABLE clientePlanes(
              	row_id VARCHAR NOT NULL,
              	planes_id INT NOT NULL,
              	PRIMARY KEY (row_id, planes_id),
              	FOREIGN KEY (row_id)
              		REFERENCES responses_df (row_id),
              	FOREIGN KEY (planes_id)
              		REFERENCES planes (planes_id)
                );"
              )
  }, silent = TRUE)
  
  
  # Create Beneficiario - Cliente table
  try({
    dbExecute(db,
              "CREATE TABLE benefcliente(
              	beneficiario_id VARCHAR NOT NULL,
              	row_id VARCHAR NOT NULL,
              	PRIMARY KEY (beneficiario_id, row_id),
              	FOREIGN KEY (beneficiario_id)
              		REFERENCES beneficiario_df (beneficiario_id),
              	FOREIGN KEY (row_id)
              		REFERENCES responses_df (row_id),
              	fecha TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
                );"
    )
  }, silent = TRUE)
  
  
  return(responses_df)
}

