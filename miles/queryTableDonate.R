queryTableDonate <- function(db){
  tableD <- dbGetQuery(db, sprintf("
                                  SELECT
                                  	r.nombre,
                                  	r.cedula,
                                  	p.nombre,
                                  	p.cedula,
                                  	p.millasrecibidas,
                                  	c.millas,
                                  	c.fecha
                                  FROM
                                  	responses_df r
                                  INNER JOIN benefcliente c
                                  	ON r.row_id = c.row_id
                                  INNER JOIN beneficiario_df p
                                  	ON c.beneficiario_id = p.beneficiario_id;
                                  "))
  tableD
}