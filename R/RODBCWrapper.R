#' Perform an SQL query
#' 
#' Perform an SQL query
#' @param connection An open RODBC connection
#' @param statement A valid SQL statement
#' @return A data frame or an invisible -2 if the statement does
#' not result in returned data.
#' @keywords internal
SqlQuery <- function (connection,statement) {
  out<-RODBC::sqlQuery (connection,statement,errors=F,
                        stringsAsFactors=F)
  if (length(out)==1 && !is.na(out) && out==-1) {
    stop(
      paste("Statement: ",statement,"\n",
            RODBC::odbcGetErrMsg(connection)))
  }
  return (out)
}

#' Open an ODBC connection
#' 
#' Open an ODBC connection
#' @param connection_string A valid ODBC connection string
#' @return An RODBC connection object
#' @export
getODBCConnection <- function (connection_string) {
  RODBC::odbcDriverConnect(connection_string)
}
#' Close an open ODBC connection
#' 
#' Close an open ODBC connection
#' @param cn An open RODBC connection
#' @export
closeODBCConnection <- function (
  cn
  ) {
  RODBC::odbcClose(cn)
}
#' Close all open ODBC connections
#' 
#' Close all open ODBC connections
#' @export
closeAllODBCConnections <- function (
  ) {
  RODBC::odbcCloseAll()
}



