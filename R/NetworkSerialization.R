#---------------------------------
# Primary Function (1): Get Network
#---------------------------------
#' Retrieve a network from a connection
#' 
#' Retrieve a network from a connection
#' @param cn An open RODBC connection
#' @param name The name of the network to retrieve
#' @param connection_string Connection string to database.
#' @param external_path Path to external inference executables
#' @param dbinfo A database information object
#' @return The retrieved bayesnet
#' @export
#---------------------------------
getNetwork <- function (
  cn,
  name,
  connection_string=NULL,
  external_path=NULL,
  dbinfo=dbmsinfo()
  ) {    
  # Get network information from DB
  # Network | DataTables | DataViews | ResultTables | Images | Datamap | Layout 
  netDF<-SqlQuery(cn,paste("SELECT * FROM ",dbinfo$networks," WHERE Network='",name,"'",sep=""))
  if (nrow(netDF)!=1) stop("Could not fetch network.")
  
  # Unserialize tableinfo
  tableinfo=restoreTableInfo(cn,netDF[1,],dbinfo)
  
  # Unserialize layout
  s=parseWSSS_asIntegerVector(netDF[1,7])
  lo=NULL
  if(!is.null(s)) lo=matrix(s,ncol=2,byrow=T)
  
  # Create bayesnet
  out<-list(
    name=name,
    nodes=list(),
    layout =lo,
    tableinfo=tableinfo,
    inf_manager=inference_management(name,external_path,connection_string)
  )  
  class(out)<-'bayesnet'
  
  # Load network nodes
  df<-SqlQuery(cn,paste("SELECT * FROM ",dbinfo$nodes,
                        " WHERE Network='",name,"' ORDER BY NodeIndex",sep=""))
  out$nodes<-apply(df,1,unserializeNode)

  # Return restored network
  return (out)
}
#---------------------------------

#---------------------------------
# Major subcomponent (1.1): Unserialize Node
#---------------------------------
#' Unserialize Node
#' 
#' Unserialize Node
#' @param row NetworkDataFrame row. See getNetwork
#' @return The unserialized node
#' @keywords internal
#---------------------------------
unserializeNode <- function (
  row
) {
  # Parse dimensions
  dimensions=parseWSSS_Dimensions(row[[11]])
  
  # Unserialize basic node features
  out<-list(
    name=row[[3]],
    index=strtoi(stringr::str_trim(row[[2]]))+1,
    values=unlist(strsplit(stringr::str_trim(row[[5]]), ",")),
    parents=parseWSSS_asIntegerVector(row[[6]])+1,
    pvals=parseWSSS_asIntegerVector(row[[7]])
  )
  
  # Set class
  class(out)<-row[[4]]
  
  # Unserialize Parameters
  out<-unserializeParameters(
    out,
    parseWSSS_Params(row[[8]],dimensions[1:2]), # Parameters1
    parseWSSS_Params(row[[9]],dimensions[3:4]), # Parameters2
    parseWSSS_Params(row[[10]],dimensions[5:6]), # Parameters3
    row[[12]])                                  # miscString
  
  # Return node
  return (out)
}
#---------------------------------



#---------------------------------
# Help Function (1.1.1&): Parse W(hite) S(pace) S(eparated) S(trings) Params
#---------------------------------
#' Parse W(hite) S(pace) S(eparated) S(trings) Params
#' 
#' Parse W(hite) S(pace) S(eparated) S(trings) for Parameters.
#' @param str String to be parsed.
#' @param dim dimension information
#' @return Parameter matrix
#' @keywords internal
#---------------------------------
parseWSSS_Params <- function (str,dim) {
  str<-stringr::str_trim(str)
  if (dim[1]==0) {return (NULL)}
  if (is.na(str)) {return (NULL)}
  if (length(str)==0) {return (NULL)}
  v<-as.numeric(unlist(strsplit(toString(str), " ")))
  if (any(is.na(v))) stop("NAs detected in parseWSSS_Params")
  matrix(v,nrow=dim[1],byrow=T)
}
#---------------------------------
# Help Function (1.1.2&): Parse W(hite) S(pace) S(eparated) S(trings) as integer vector
#---------------------------------
#' Parse W(hite) S(pace) S(eparated) S(trings) as integer vector
#' 
#' Parse W(hite) S(pace) S(eparated) S(trings) as integer vector.
#' @param str String to be parsed
#' @return Integer Vector
#' @keywords internal
#---------------------------------
parseWSSS_asIntegerVector <- function (str) {
  str<-stringr::str_trim(str)
  if (is.null(str)) {return (NULL)}
  if (is.na(str)) {return (NULL)}
  if (length(str)==0) {return (NULL)}
  out<-strtoi(unlist(strsplit(toString(str), " ")))
  if (any(is.na(out))) {stop("NAs detected in parseWSSS_asIntegerVector")}
  return (out)
}
#---------------------------------
# Help Function (1.2): Parse W(hite) S(pace) S(eparated) S(trings) as String Vector
#---------------------------------
#' Parse W(hite) S(pace) S(eparated) S(trings) as string vector
#' 
#' Parse W(hite) S(pace) S(eparated) S(trings) as string vector.
#' @param str String to be parsed
#' @return String Vector
#' @keywords internal
#---------------------------------
parseWSSS_asStringVector <- function (str) {
  str<-stringr::str_trim(str)
  if (is.null(str)) {return (NULL)}
  if (is.na(str)) {return (NULL)}
  if (length(str)==0) {return (NULL)}
  out<-unlist(strsplit(toString(str), " "))
  if (any(is.na(out))) {stop("NAs detected in parseWSSS_asStringVector")}
  return (out)
}
#---------------------------------
# Help Function (1.1.3&): Parse W(hite) S(pace) S(eparated) S(trings) for Dimensions
#---------------------------------
#' Parse W(hite) S(pace) S(eparated) S(trings) for Dimensions
#' 
#' Parse W(hite) S(pace) S(eparated) S(trings) for Dimensions
#' @param str String to be parsed
#' @return 6 item integer vector
#' @keywords internal
#---------------------------------
parseWSSS_Dimensions<-function(str){
  str<-stringr::str_trim(str)
  if (is.null(str) || is.na(str) || length(str)==0) stop("Invalid dimensions length!")
  out<-strtoi(unlist(strsplit(str, " ")))
  if (length(out)!=6)  stop("Invalid dimensions length!")
  if (any(is.na(str))) stop ("NAs detected in parseWSSS_Vector_String")
  return (out)
}
#---------------------------------


encodeWSSS <- function (m) {
  if (is.null(m)||length(m)==0) {return ("NULL")}
  if (any(is.na(m))) {stop("NAs detectedin encodeWSSS")}
  paste("'",paste(t(m),collapse=" "),"'",sep="")
}
encodeDimensions <- function (Parameters1,Parameters2,Parameters3) {
  paste("'",
        ifelse(is.null(Parameters1)||length(Parameters1)==0,0,nrow(Parameters1)),
        ifelse(is.null(Parameters1)||length(Parameters1)==0,0,ncol(Parameters1)),
        ifelse(is.null(Parameters2)||length(Parameters2)==0,0,nrow(Parameters2)),
        ifelse(is.null(Parameters2)||length(Parameters2)==0,0,ncol(Parameters2)),
        ifelse(is.null(Parameters3)||length(Parameters3)==0,0,nrow(Parameters3)),
        ifelse(is.null(Parameters3)||length(Parameters3)==0,0,ncol(Parameters3)),
        "'",sep=" ")
}
serializeNodes <- function (
  node,
  netName,
  nodesTable,
  cn
  ) {
  Parameters1 <- getParameters1(node)
  Parameters2 <- getParameters2(node)
  Parameters3 <- getParameters3(node)  
  statement <- paste("INSERT INTO ",nodesTable," VALUES (",
                     "'",netName,"' , ",
                     node$index-1," , ",
                     "'",node$name,"' , ",
                     "'",class(node),"' , ",
                     "'",paste(node$values,collapse=","),"' , ",
                     encodeWSSS(node$parents-1)," , ",
                     encodeWSSS(node$pvals)," , ",
                     encodeWSSS(Parameters1)," , ",
                     encodeWSSS(Parameters2)," , ",
                     encodeWSSS(Parameters3)," , ",
                     encodeDimensions(Parameters1,Parameters2,Parameters3)," , ",
                     getMiscString(node),")",
                     sep=""
  )
  SqlQuery (cn,statement)
}


