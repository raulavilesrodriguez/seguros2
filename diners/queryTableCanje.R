queryTableCanje <- function(db){
  tableD <- dbGetQuery(db, sprintf("
                                  SELECT
                                  	b.nombre,
                                  	b.cedula,
                                  	b.email,
                                  	b.estado,
                                  	b.millasrecibidas,
                                  	d.millascanjeadas,
                                  	d.fecha
                                  FROM
                                  	beneficiario_df b
                                  INNER JOIN diners d
                                  	ON b.beneficiario_id = d.beneficiario_id;
                                  "))
  tableD
  
}