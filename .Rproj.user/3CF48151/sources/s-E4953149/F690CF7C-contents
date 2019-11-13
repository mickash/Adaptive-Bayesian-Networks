#--------------------
# Principle Function (1): Bayes net constructor
#--------------------
#' Bayes net constructor
#' 
#' Bayes net constructor. Nodes are added using 'addNode' function.
#' The net object includes the layout field, which you should edit to control 
#' plot layouts of the network. It is at $layout and is 
#' a matrix specifying the plot coordinates of the nodes.
#' If you wish to add external_path and connection_string at a later point, you can
#' access these at $inf_manager$external_path and $inf_manager$connection_string. If they
#' are null, you will not be able to run the external inference engine and other 
#' external apps.
#' @param name The name of the new network
#' @param dbinfo A database information object, returned from intializeDatabase. If not provided,
#' a default object will be created, with networks="networks", nodes="nodes", log="log", and metadata="metadata"
#' @param external_path Path to external inference executables
#' @param connection_string Connection string to database.
#' @param table_length The number of variables to have in data and result tables.
#' @return new Bayesian network
#' @export
#--------------------
bayesnet <- function (
  name,
  dbinfo=dbmsinfo(),
  connection_string=NULL,
  external_path=NULL,
  table_length=200
  ) {
  out<-list(
    name=name,
    nodes=list(),
    layout=c(),
    tableinfo=createTableInfo(name,table_length,dbinfo),
    inf_manager=inference_management(name,external_path,connection_string)
    )
  class(out)<-'bayesnet'
  return (out)  
} 
#--------------------
# Principle Function (2): Bayes net cloner
#--------------------
#' Bayes net cloner
#' 
#' Copies passed net'Bayesnet's nodes and layout, and gives 
#' network the passed name. 
#' Also copies the data master tables names and the variable map onto these tables.
#' @param net Network to clone
#' @param name Name for new network
#' @return new Bayesian network
#' @export
#--------------------
cloneBayesnet <- function (net,name) {
  out<-bayesnet(name,net$dbinfo)
  out$nodes=net$nodes
  out$layout=net$layout
  out$tableinfo=cloneTableInfo(name,net$tableinfo)
  out$inf_manager=clone_inference_management(name,net$inf_manager)
  return (out)  
}
#--------------------
