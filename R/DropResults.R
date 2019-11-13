#------------------------
# Principle Function (1): Delete Results
#------------------------
#' Delete results
#'  
#' Delete results from a network's results table.
#' @param cn An open RODBC connection
#' @param net The network
#' @param first The first item to delete. All subsequent items will be deleted.
#' @keywords internal 
#------------------------
dropResults <- function (cn,net,first) {
  # Set autocommit
  RODBC::odbcSetAutoCommit(cn,FALSE) 
  
  # Drop results
  for (table in net$tableinfo$result_tables) 
    SqlQuery(cn,paste("DELETE from ",table," WHERE item >= ",first,sep=""))        
  
  # Reset autocommit
  RODBC::odbcSetAutoCommit(cn,TRUE) 
}
#------------------------
