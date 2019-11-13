#------------------------------
# Primary Function (1): Read Data
#------------------------------
#' Read data (evidence) from a network's data table
#' 
#' Read data (evidence) into a network's data table
#' @param connection An open RODBC connection
#' @param net The network, with a valid data table name in
#' the dataTable field - so network tables must have been
#' initialized.
#' @param item The first or only row to read.
#' @param itemTo The last row to read, or NA.
#' at the location.
#' @param zeroindexed Specifies if returned values 
#' should be zero indexed.
#' @return A dataframe consisting of the specified rows
#' @export
#------------------------------
readData <- function (
  cn,
  net,
  item,
  itemTo=NA,
  zeroindexed=F
  ) {  
  # Select variables
  str<-paste("SELECT",make_v_Strings(length(net$nodes)))
  
  # From clause
  str<-paste(str,' FROM ',paste(net$tableinfo$data_views,collapse=','),sep="")
  
  # Where clause
  str<-paste(str,' WHERE ',sep="")
  if (is.na(itemTo)) str<-paste(str,"item=",item)
  else str<-paste(str,"item>=",item," AND item <=",itemTo)    

  # Make query and use empty data frame if no rows
  df<-SqlQuery(cn,str)
  if (length(df)==1 && df==-2) df<-data.frame()

  # Adjust for -1 to NA and 1-indexed
  df[which(as.matrix(df)==-1)]=NA
  if (!zeroindexed) df<-df+1
  
  # Return data
  return (df)
}
#------------------------------

#------------------------------
# Help function (1&.1): Make V-String
#------------------------------
#' Make V-Strings
#' 
#' Make V-Strings
#' @param number The number of v-strings to make
#' @param base The base to add to the numbers
#' @return A string of the form "v_1,v_2,...,v_number".
#' @keywords internal
#------------------------------
make_v_Strings=function(number,base=0) {
  str=""
  for (i in 1:number) {
    if (i!=1) str<-paste(str,',',sep="")  
    str<-paste(str,paste('v_',i+base,sep=""))
  }  
  return (str)
}
#------------------------------


