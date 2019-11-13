#-------------------------------
# Primary Function (1): Read Results
#-------------------------------
#' Get the posterior distributions for a network for a data item.
#' 
#' Get the posterior distributions for a network for a data item.
#' @param connection An open RODBC connection
#' @param net The network, with a valid results table name in
#' the resultsTable field - so network tables must have been
#' initialized.
#' @param item The item to retrieve.
#' @return A posterior probabilities object 
#' @export
#-------------------------------
readResults <- function (
  cn,
  net,
  item
  ) {
  # Select variables
  str<-paste("SELECT",make_vv_Strings(net))  

  # From clause
  str<-paste(str,' FROM ',paste(net$tableinfo$result_tables,collapse=','),sep="")  
  
  # Where clause
  str=paste(str," WHERE item=",item,sep="")
  
  # Perform query
  df<-SqlQuery(cn,str)
  
  # Check results were found
  if (length(df)==1 && df==-2)
    stop(paste("No results found for item: "),item,".",sep="")  
  
  # Write results into network object
  index=1
  posterioris=list()
  for (i in 1:length(net$nodes)) {
    posterioris[[i]]<-df[1,index]
    index=index+1
    for (j in 2:length(net$nodes[[i]]$values)) {
      posterioris[[i]]<-
        append(posterioris[[i]],df[1,index])
      index=index+1
    }
  }
  
  # Return posterior object
  return (posterioris)
}
#-------------------------------
# Primary Function (2): Check results for item
#-------------------------------
#' Check results for item.
#' 
#' Check results for item.
#' @param connection An open RODBC connection
#' @param net The network, with a valid results table name in
#' the resultsTable field - so network tables must have been
#' initialized.
#' @param item The item to retrieve.
#' @return Boolean specifying if item is present
#' @export
#-------------------------------
checkResultsForItem<-function(connection,net,item) {
  str<-paste("SELECT COUNT(*) FROM ",net$tableinfo$result_tables[1]," WHERE item=",item,sep="")
  df<-SqlQuery(connection,str)
  if (length(df)==1 && df==-2) return (FALSE)
  else return (df[1,1]==1)
}  
#-------------------------------


#-------------------------------
# Help Function (1.1): Make variable-value strings
#-------------------------------
#' Make variable-value strings
#' 
#' Make variable-value strings
#' @param net The bayesnet object
#' @return A string of the form "v_1_1,v_1_2,v_2_1,...,v_number_value".
#' @keywords internal
#-------------------------------
make_vv_Strings=function(net) {
  str=""
  for (i in 1:length(net$nodes)) {
    for (j in 1:length(net$nodes[[i]]$values)) {
      if (i!=1 || j!=1) str<-paste(str,',',sep="")
      str<-paste(str,paste('v_',i,'_',j,sep=""))
    }
  }  
  return (str)
}
#-------------------------------