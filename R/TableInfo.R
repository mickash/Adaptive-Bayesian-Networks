#--------------------
# Principle Function (1): Create table information object
#--------------------
#' Create table information object
#' 
#' Create table information object. This holds information on all database tables
#' associated with the network.
#' @param netname The name of the Bayesian network associated with this object.
#' @param table_length The number of variables to have in data and result tables.
#' @param dbinfo A database information object.
#' @return A table information object
#' @export
#--------------------
createTableInfo=function(netname,table_length=200,dbinfo) {
  out=list(
    netname=netname,
    map=NULL,
    data_family=NULL,
    data_tables=c(),
    data_views=c(),
    result_tables=c(),
    image_table=NULL,
    table_length=table_length,
    data_table_base=paste(netname,"data",sep="_"),
    result_table_base=paste(netname,"result",sep="_"),
    image_table_base=paste(netname,"image",sep="_"),
    dbinfo=dbinfo
  )
  class(out)="tableinfo"
  return (out)
}  
#--------------------
# Private semi-principle function (2): Clone table information object
#--------------------
#' Clone table information object
#' 
#' Clone table information object
#' @param newname The name of the Bayesian network the new table information object will
#' be associated with.
#' @param tableinfo The table information object to be cloned.
#' @return A new table information object
#' @keywords internal
#--------------------
cloneTableInfo=function(newname,tableinfo) {
  out=createTableInfo(newname,tableinfo$table_length,tableinfo$dbinfo) # Most fields become blank
  out$map=tableinfo$map        # Map to data table is the same as original
  out$data_family=tableinfo$data_family # Data familty remains the same
  out$data_tables=tableinfo$data_tables  # Data tables are shared
  return (out)
}
#--------------------
# Private semi-principle function (3): Unserialize table information object
#--------------------
#' Unserialize table information object
#' 
#' Unserialize table information object
#' @param cn A RODBC connection
#' @param netRow Appropriate row from the networks table. The format should be:
#' Network | DataFamily | DataTables | DataViews | ResultTables | 
#' Images | Datamap | Layout | TableLength
#' @param dbinfo A database information object.
#' @return The unserialized new table information object
#' @keywords internal
#--------------------
restoreTableInfo=function(cn,netRow,dbinfo) {
  # netRow is a row from the networks table
  out=list(
    netname=netRow[1,1],
    map=parseWSSS_asIntegerVector(netRow[1,6]),
    data_family=netRow[1,2],
    data_views=parseWSSS_asStringVector(netRow[1,3]),
    result_tables=parseWSSS_asStringVector(netRow[1,4]),
    image_table=netRow[1,5],
    table_length=netRow[1,8],
    data_table_base=paste(netRow[1,1],"data",sep="_"),
    result_table_base=paste(netRow[1,1],"result",sep="_"),
    image_table_base=paste(netRow[1,1],"image",sep="_"),
    dbinfo=dbinfo
  )

  # Get data tables from data family
  str=paste("SELECT DataTable FROM ",dbinfo$metadata," WHERE Family='",
            out$data_family,"'",sep="")
  df=SqlQuery(cn,str)
  out$data_tables=df[,1]
  
  # Return object
  class(out)="tableinfo"
  return (out)
}
#--------------------
