#------------------------
# Principle Function (1): Write Data
#------------------------
#' Enter data (evidence) into a network's data table
#' 
#' Enter data (evidence) into a network's data table. Note that calling this function when
#' the network does not include all variables in the data master will lead to such nodes being treated as unknown.
#' @param cn An open RODBC connection
#' @param evidence A vector of integers, one for each 
#' variable in the network, specifying the observed value, 
#' or NA for unknown.
#' @param net The network, with a valid data table name in
#' the dataTable field - so network tables must have been
#' initialized.
#' @param zeroindexed Specifies if values are zero indexed.
#' @param time Date-time string if you do not want current date time to
#' be specified. If used it should be in the format YYYY-MM-DD HH:MM:SS.
#' For combatability with mySQL seconds is the finest grained time permitted.
#' It is the callers responsibility to ensure that all new entries in the
#' data table have time stamps later than all previous entries. Failure to
#' do so will result in undefined behaviour when attempting a rewind-readapt.
#' @param check_tables Specifies if the metadata table should be checked to see if 
#' additional data tables have been added to the family. If this is not called, and 
#' such tables have been added, these additional tables will not have evidence
#' entered into them. Other tables will have, so the correlation of item keys between
#' tables will have been corrupted. For efficiency, do not set to TRUE if you are 
#' sure no tables could have been added.
#' @export
#------------------------
writeData <- function (
  cn,
  evidence,
  net,
  zeroindexed=F,
  time=NULL,
  check_tables=F
) {
  # Check Tables if desired
  if (check_tables) {
    count=SqlQuery(cn,"SELECT COUNT(*) FROM ",tableinfo$dbinfo$metadata,
                             " WHERE Family='",tableinfo$data_family,"'")[1,1]
    total=length(tableinfo$data_tables)
    for (i in (count+1):total) 
      tableinfo$data_tables=c(tableinfo$data_tables,paste(tableinfo$data_family,"_",i,sep=""))
  }
  
  # Set autocommit
  RODBC::odbcSetAutoCommit(cn,FALSE)  
  
  # Adjust for zero-indexing and NAs as -1
  if (!zeroindexed) evidence=evidence-1 
  evidence[is.na(evidence)]=-1
  
  lim=length(net$tableinfo$data_tables)
  for (i in 1:lim) {
    # Find variables mapped to this result table
    first=(i-1)*net$tableinfo$table_length+1
    last=min(i*net$tableinfo$table_length,length(net$nodes))
    subset=which(net$tableinfo$map>=first & net$tableinfo$map<=last)
    
    # Find length of table
    len=net$tableinfo$table_length 
    if (i==lim) len=SqlQuery(cn,paste("SELECT TableLength FROM ",net$tableinfo$dbinfo$metadata,
                   " WHERE DataTable='",net$tableinfo$data_tables[i],"'",sep=""))[1,1]
    
    # Create new evidence vector for table
    new_evidence=rep(-1,len)
    new_evidence[net$tableinfo$map[subset]%%net$tableinfo$table_length]=evidence[subset]

    # Calculate base
    base=(i-1)*net$tableinfo$table_length
    
    # Start statement
    str<-paste("INSERT INTO ",net$tableinfo$data_tables[i]," ( ",sep="")
    
    # Specify variable columns
    str=paste(str,make_v_Strings(length(new_evidence),base))
    
    # Specify time column if give
    if (!is.null(time))  str<-paste(str,', time',sep="")    
    
    # End columns, specify values
    str<-paste(str,') VALUES (',paste(new_evidence,collapse=','),sep="")
    
    # Add time if given.
    if (!is.null(time)) str<-paste(str,', "',time,'"',sep="")          
    
    # End statement 
    str<-paste(str,')',sep="")
    
    # Run query 
    SqlQuery(cn,str)    
  }
  
  # Set autocommit
  RODBC::odbcSetAutoCommit(cn,TRUE)  
}
#------------------------
# Private semi-principle function (2): Update data
#------------------------
#' Update data
#' 
#' Update data in a network's data table by setting a particular variable
#' to a particular value for a specific set of rows.
#' @param cn An open RODBC connection
#' @param net The network
#' @param first The first item to delete. All subsequent items will be deleted.
#' @param variableIndices The indices of the variables
#' to change.
#' @param values The values to set the specified variables 
#' to. 
#' @keywords internal 
#------------------------
updateData<-function(
  cn,
  net,
  first,
  last,
  variableIndices,
  values
  ) {
  map=net$tableinfo$map[variableIndices]
  
  lim=length(net$tableinfo$data_tables)
  for (i in 1:lim) {
    # Find variables mapped to this result table
    first=(i-1)*net$tableinfo$table_length+1
    last=min(i*net$tableinfo$table_length,length(net$nodes))
    subset=map[which(map>=first & map<=last)]
      
    str<-paste("UPDATE ",net$tableinfo$data_tables[i]," SET ",sep="")
    
    for (s in subset) {
      if (s!=subset[1]) str<-paste(str,",")
      str<-paste(str,"v_",variableIndices[s]," = ",
               values[s]-1,
               sep="")
    }
    str<-paste(str," WHERE item>=",first," AND item<=",
             last,sep="")
  
    SqlQuery(cn,str)    
  }
}
#------------------------
