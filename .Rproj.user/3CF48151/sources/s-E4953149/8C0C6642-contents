#-------------------
# Principle Function (1): Create Database Information Object
#-------------------
#' Create database information object.
#' 
#' Create database information object. This contains names for all location tables.
#' @param networks The name of the networks table.
#' @param metadata The name of the metadata table.
#' @param nodes The name of the nodes table.
#' @param log The name of the log table.
#' @param metaresults The name of the metaresults table.
#' @return A database information object.
#' @export
#-------------------
dbmsinfo=function(
  networks="networks",
  metadata="metadata",
  nodes="nodes",
  log="log",
  metaresults="metaresults"
) {
  out=list(
    metadata="metadata",
    networks="networks",
    nodes="nodes",
    log="log",
    metaresults="metaresults"
  ) 
  class(out)="dbmsinfo"
  return(out)
}
#-------------------


