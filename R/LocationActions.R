#-----------------
# Primary Function (1): List networks in database
#-----------------
#' List networks in database
#' 
#' List networks in database
#' @param cn An open RODBC connection
#' @param networks The networks table in the database
#' @return A vector specifying names of available networks.
#' @export
#-----------------
getNetworks <- function (
  cn,
  networks="networks"
  ) {
  as.vector(
    SqlQuery(cn,paste("SELECT * from ",networks,sep=""))[,1]
  )
}
#-----------------
# Primary Function (2): List data families in database
#-----------------
#' List data families in database
#' 
#' List data families in database
#' @param cn An open RODBC connection
#' @param metadata The metadata table in the database
#' @return A vector specifying names of available networks.
#' @export
#-----------------
getDataFamilies <- function (
  cn,
  metadata="metadata"
) {
  unique(as.vector(
    SqlQuery(cn,paste("SELECT * from ",metadata,sep=""))[,2]
  ))
}
#-----------------
# Primary Function (3): Get log entries
#-----------------
#' Get log entries
#' 
#' Get log entries
#' @param cn An open RODBC connection
#' @param log The log table in the database
#' @return A data frame of the log entries
#' @export
#-----------------
getLogEntries <- function (
  cn,
  log="log"
) {
  SqlQuery(cn,paste("SELECT * from ",log,sep=""))
}
#-----------------
