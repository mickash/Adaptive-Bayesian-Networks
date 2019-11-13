#-------------------
# Principle Function (1): Rewind and Readapt given feedback
#-------------------
#' Rewind and Readapt given feedback
#' 
#' Given user feedback that a particular variable took a particular value
#' between times t and u this function will:
#' 1. Restore the image closest to but prior to t.
#' 2. Change all data between t and s such that the variable at the specified
#'    index takes the specified value.
#' 3. Remove all results of items occurring after the time of the 
#'    restored image.
#' The rewound'  network will be returned. No readaption will have yet occurred.
#' All external inference sessions should be stopped before running this function.
#' @param cn An open RODBC connection
#' @param net The network. The network must have a valid image table name 
#' in the imageTable field - so network tables must have been initialized.
#' @param from The time from which the data alteration is to occur. This 
#' should be a valid date-time string. For MySQL this is YYYY-MM-DD HH:MM:SS.
#' @param to The time to which the data alteration is to occur. This 
#' should be a valid date-time string. For MySQL this is YYYY-MM-DD HH:MM:SS.
#' @param variableIndices The indices of the variables 
#' whose values are known in the specified interval.
#' @param values The values for the above variables.
#' @return The network set to the state of the appropriate prior image. No readaption will have
#' yet occurred. 
#' @export
#-------------------
rewindReadapt<-function(
  cn,
  net,
  from,
  to,
  variableIndices,
  values
) {
  # Check no external inference sessions running.
  if(length(net$inf_manager$running_ids)>0)
    stop("Stop the external inference session associated with this network before restoring image.")
  
  # Set autocommit
  RODBC::odbcSetAutoCommit(cn,FALSE)
  
  # Find first to alter
  first<-getClosestToTime(cn,net$tableinfo$data_views[1],from,"item",
                          prior=T,secondaryOrder="item")
  
  # Find last to alter
  # We use 'Desc' below to deal with technically 
  # unsupported/invalid case where multiple items 
  # have the same time stamp.
  last<-getClosestToTime(cn,net$tableinfo$data_views[1],to,"item",
                         prior=T,secondaryOrder="item Desc")
  
  # Update data
  updateData(cn,net,first,last,variableIndices,values)
  
  # See if results had changed items
  if (checkResultsForItem(cn,net,first)) {
    # Find time before result item=first
    str<-paste("SELECT time FROM ",net$tableinfo$result_tables[1],
               " WHERE item=",first,sep="")
    df<-SqlQuery(cn,str)    
    if (length(df)==0) {
      # Reset autocommit
      RODBC::odbcSetAutoCommit(cn,TRUE)
      stop("The result item time was not retrieved. The reasons for the failure are unknown.")
    }
    
    # Restore the image closest to, but prior to, that
    # time. Record the time of that image 
    iTime<-restoreImage_internal(cn,net,df[1,1])
    
    # Find the result nearest but later than 
    # the image storage time.
    # We fetch the last such item and accept that time 
    # granularity may cause some problems. 
    rFirst<-getClosestToTime(cn,net$tableinfo$result_tables[1],iTime,
                             "item",prior=F,
                             secondaryOrder="item")
    
    # Drop results of altered data items
    dropResults(cn,net,rFirst)  
    
    # Drop any later images
    str=paste("DELETE FROM ",net$tableinfo$image_table," WHERE Time > '",iTime,"'",sep="")
    SqlQuery(cn,str)
  } 
  else {
    # The data hasn't been processed yet, so do nothing
    # :)    
  }
  
  # Load network
  net=getNetwork(cn,net$name,net$inf_manager$connection_string,net$inf_manager$path)
  
  # Reset autocommit
  RODBC::odbcSetAutoCommit(cn,TRUE)
  
  # Return network
  return (net)
}
#-------------------

#-------------------
# Help function (1.1): Get value from time
#-------------------
#' Get a value from a time
#' 
#' Get the value in column col that is in the row whose 
#' timeCol timestamp is closest but prior to time from table. 
#' @param cn An open RODBC connection
#' @param table The table name
#' @param time The time the entry should be closest but prior to/after
#' @param col The column of the entry to return
#' @param timeCol The column where the time stamp to compare is.
#' @param prior If the compared value must be prior to (T) or after (F)
#' @param secondaryOrder A secondary column to order on.
#' @keywords internal 
#-------------------
getClosestToTime<-function(cn,table,time,col,
                           timeCol="time",
                           prior=T,secondaryOrder=NULL) {
  # Set comparison and order direction(s)
  comp<-" >= "
  word<-" "
  if (prior) {
    comp<-" <= "
    word<-" Desc "
  }
  if (!is.null(secondaryOrder)) 
    word<-paste(word,",",secondaryOrder)
  
  # Find closest
  str<-paste("SELECT ",col," FROM ",table,
             " WHERE ",timeCol,comp,"'",time,
             "' ORDER BY ",timeCol," ",word," LIMIT 1",
             sep="")
  df<-SqlQuery(cn,str)
  
  # Check something was found
  if (length(df)==1 && df==-2) 
    stop(paste("No results found for time: "),time," in table ",table,".",sep="")  
  
  # Return closest 
  return (df[1,1])  
}
#-------------------
