#---------------------
# Principle Functionality (1): Add node to network
#---------------------
#' Add node to network
#' 
#' Adds a new node of to the network. 
#' NOTE: RESULTS WILL BE CLEARED. If you want to have access to past results you must copy and
#' store them elsewhere before calling this function.
#' NOTE: ADDING A NODE CAN RESULT IN A NEW DATA TABLE BEING CREATED.
#' @param net the current network
#' @param type string specifying type of node to add. Valid values are: 
#' 'noisyor', 'dirichlet', 'logand', 'logor', logprod', 'flag',
#' and 'flagprod'.
#' @param values vector of strings specifying the names of the values of the 
#' node. Note that all nodes must have at least two values, and noisy-or and 
#' logical-and type nodes can only have two values.
#' @param name string specifying the name of the node.
#' @param parents vector of integers specifying the indices of the parents of 
#' node. Note that logical-and nodes must have exactly two parents.
#' @param params matrix specifying parameters of node.
#' @param params2 matrix specifying secondary parameters of node.
#' @param miscString a string specifying additional settings.
#' Currently used only for multinomial noisy-or nodes, in
#' which case it should be 'T' if the secondary parameters
#' matrix should be adaptable, and 'F' otherwise.
#' @param cn A RODBC connection. Required if performing deep update.
#' @param deepUpdate Specifies whether a deep update should be performed.
#' @param dropImages Specifies if images should be dropped on a deepUpdate.
#' @return new Bayesian network with node added.
#' @export
#---------------------
addNode <- function (
  net,
  type,
  values,
  name,
  parents,
  params,
  params2 = NULL,
  miscString = NULL,
  cn=NULL,
  deepUpdate=F,
  dropImages=F
) {
  # Validate parent indices
  if (length(parents)>0 && (max(parents)>length(net$nodes) || min(parents)<0)) stop('Invalid parent index.')

  # Validate connection present if deep update desired
  if (deepUpdate && is.null(cn)) stop ("Connection required for deep update.")
  
  # Perform shallow update
  net=shallowUpdateOnNodeAdded(net,type,values,name,parents,params,params2,miscString)
  
  # Perform deep update
  if (deepUpdate) net=deepUpdateOnNodeAdded(cn,net,dropImages)
  
  # Return modified network
  return (net)
}
#---------------------

#---------------------
# Major Sub-components (1.1): Shallow Updating - Add node to R Bayesnet object
#---------------------
#' Add node to R Bayesnet object
#' 
#' Add node to R Bayesnet object
#' @param net the current network
#' @param type string specifying type of node to add. Valid values are: 
#' 'noisyor', 'dirichlet', 'logand', 'logor', logprod', 'flag', and 'flagprod'.
#' @param values vector of strings specifying the names of the values of the 
#' node. Note that all nodes must have at least two values, and noisy-or and 
#' logical-and type nodes can only have two values.
#' @param name string specifying the name of the node.
#' @param parents vector of integers specifying the indices of the parents of 
#' node. Note that logical-and nodes must have exactly two parents.
#' @param params matrix specifying parameters of node.
#' @param params2 matrix specifying secondary parameters of node.
#' @param miscString a string specifying additional settings.
#' Currently used only for multinomial noisy-or nodes, in
#' which case it should be 'T' if the secondary parameters
#' matrix should be adaptable, and 'F' otherwise.
#' @return new Bayesian network with node added.
#' @keywords internal
#---------------------
shallowUpdateOnNodeAdded=function(net,type,values,name,parents,params,params2 = NULL,miscString = NULL)
{
  pvals=get.values.bayesnet(net,parents)
  index <- length(net$nodes)+1
  node=NULL
  if (type=='noisyor') {
    node <- noisyor(
      index=index,
      values=values,
      name=name,
      parents=parents,
      pvals=pvals,
      params=params,
      params2=params2,
      updateDisambiguation=miscString
    )
  } else if (type=='dirichlet') {
    node <- dirichlet(
      index=index,
      values=values,
      name=name,
      parents=parents,
      pvals=pvals,
      params=params,
      params2=params2
    )
  } else if (type=='logand') {
    node <- logand(
      index=index,
      name=name,
      values=values,
      parents=parents,
      pvals=pvals,
      params=params,
      params2=params2
    )
  } else if (type=='flag') {
    node <- flag(
      index=index,
      name=name,
      values=values,
      parents=parents,
      pvals=pvals,
      params=params,
      params2=params2
    )
  } else if (type=='logprod') {
    node <- logprod(
      index=index,
      name=name,
      values=values,
      parents=parents,
      pvals=pvals,
      params=params,
      params2=params2
    )
  }else if (type=='flagprod') {
    node <- flagprod(
      index=index,
      name=name,
      values=values,
      parents=parents,
      pvals=pvals,
      params=params,
      params2=params2
    )
  }else if (type=='logor') {
    node <- logor(
      index=index,
      name=name,
      values=values,
      parents=parents,
      pvals=pvals,
      params=params,
      params2=params2
    )
  } else {
    stop("Invalid node type.")
  }
  net$nodes <- append(net$nodes,list(node))
  return (net)
}
#---------------------
# Major Sub-component (1.2): Deep Updating - Add node to DB 
#---------------------
#' Perform a deep update after adding a node
#' 
#' Update the images and tables of a network after adding a node.
#' @param cn An open RODBC connection.
#' @param net The network.
#' @param dropImages Whether images should be dropped or updated. 
#' @return The updated network
#' @keywords internal
#----------------------
deepUpdateOnNodeAdded<-function(
  cn,
  net,
  dropImages=F
) {
  # Set autocommit
  RODBC::odbcSetAutoCommit(cn,FALSE)    
  
  # Add new variable to tableinfo and master data tables if required 
  # The new node will be at the end of the nodes vector.
  net$tableinfo=addOrRestoreVariable(cn,net$tableinfo,net$nodes[[length(net$nodes)]]$name,
                                     length(net$nodes[[length(net$nodes)]]$values))
  
  # Update network datamap in DB (datamap updated in addOrRestoreVariable)
  updateDatamapInDB(cn,net)
  
  # Update nodes in network
  updateSerializedNetwork(cn,net,T)
  
  # Update network images
  updateImagesOnNodeAdded(cn,dropImages,net)
  
  # No need to update other networks working with this data table 
  # Their datamaps and views are still valid
  
  # Update data view
  net=recreateDataView(cn,net,T)
  
  # Update results table
  net=recreateResultTables(cn,net,T)
  
  # Reset autocommit
  RODBC::odbcSetAutoCommit(cn,TRUE)      
  
  # Return network
  return(net)
}
#------------------

#------------------
# Help function (1.1.1): Get the number of values for the specified nodes
#------------------
#' Get the number of values for the specified nodes
#' 
#' Get the number of values for the specified nodes
#' @param net the network
#' @param pars vector of indices of the nodes to get information about
#' @return vector of integers specifying the number of values for the 
#' specified nodes
#' @keywords internal
#------------------
get.values.bayesnet <- function (net,pars) {
  get.values.node <- function (index,net) {
    length(net$nodes[[index]]$values)
  }
  unlist(lapply(pars,get.values.node,net))
}
#------------------
# Help function (1.2.1): Add or restore variable to data tables
#------------------
#' Add or restore variable to data tables
#' 
#' Add or restore variable to data tables.
#' Checks if the new variable to be added is already present in the master data tables.
#' If so, and it is valid (the desired values match stored values) it is 'restored' which
#' simply means adding it to the datamap.
#' Otherwise the variable is added to the master data tables and then to the map.
#' @param cn An RODBC connection
#' @param tableinfo A table information object
#' @param newNode The name of the new variable
#' @param newValues The number of values the new variable has
#' @return The update tableinfo object
#' @keywords internal
#------------------
addOrRestoreVariable=function(
  cn,
  tableinfo,
  newNode,
  newvalues
  ) {
  # Check if we have re-added a node previously excluded
  # Metadata table has information on names of nodes in the data table. These must be unique.  
  names_and_values=getVariableNamesAndValuesInDataTables(cn,tableinfo)
  masterNames=names_and_values[[1]]
  masterValues=as.numeric(names_and_values[[2]])
  if (newNode %in% masterNames) {
    # Check if values match. If not, stop with message
    if (masterValues[which(masterNames==newNode)]!=newvalues)
      stop ("Name matches currently ignored column in master data table, but values mismatch. No updates performed.")
    # Update network object datamap 
    tableinfo$map=c(tableinfo$map,which(masterNames==newNode))
    # No need to update master table 
  } 
  else {
    # Update network object datamap 
    tableinfo$map=c(tableinfo$map,length(masterNames)+1)    
    # Update master table 
    addVariableToDataMaster(cn,tableinfo,newNode,newvalues)
  }
  return (tableinfo)  
}
#---------------------
# Help function (1.2.2): Add Variable To Data Master
#---------------------
#' Add variable to master data tables
#' 
#' Add variable to master data tables
#' @param cn connection
#' @param tableinfo Table information object
#' @param name Name of new node
#' @param values Number of values of new node 
#' @keywords internal
#---------------------
addVariableToDataMaster<-function(
  cn,
  tableinfo,
  name,
  values
){
  last_table_index=length(tableinfo$data_tables)
  last_table=tableinfo$data_tables[last_table_index]
  
  # Check if there is room in the final data table.
  str=paste("SELECT TableLength FROM ",tableinfo$dbinfo$metadata,
            " WHERE DataTable='",last_table,"'",sep="")
  cols=SqlQuery(cn,str)[1,1]
  
  # If there is not, we create a new table and add it to the metadata table 
  # under with the specified data family
  if (cols==tableinfo$table_length) {
    table_name=createDataTable(cn,tableinfo$data_family,
                               last_table_index+1,1,tableinfo$table_length)
    tableinfo$data_tables=c(tableinfo$data_tables,table_name)      
    addInformationToMetaData(cn,tableinfo$dbinfo$metadata,tableinfo,net,
                             table_name,last_table_index+1,1,tableinfo$table_length)
  }

  # Otherwise we alter the last table to have one more variable.
  else {
    base=(last_table_index-1)*tableinfo$table_length
    str=paste("ALTER TABLE ",last_table," ADD COLUMN v_",
              base+cols+1," INT DEFAULT -1 AFTER v_",base+cols,sep="")
    SqlQuery(cn,str)    
    
    # Update information in metadata
    str=paste("SELECT NodeNames,NodeValues FROM ",tableinfo$dbinfo$metadata,
              " WHERE DataTable='",last_table,"'",sep="")
    df=SqlQuery(cn,str)
    masterNames=parseWSSS_asStringVector(df[1,1])
    masterNames=c(masterNames,name)
    newMasterNames=encodeWSSS(masterNames)
    masterValues=parseWSSS_asIntegerVector(df[1,2])
    masterValues=c(masterValues,toString(values))
    newMasterValues=encodeWSSS(masterValues)
    newTableLength=cols+1
    str=paste("UPDATE ",tableinfo$dbinfo$metadata," SET NodeNames=",newMasterNames,
              ",NodeValues=",newMasterValues,",TableLength=",newTableLength,
              " WHERE DataTable='",last_table,"'",sep="")
    SqlQuery(cn,str)    
  }
}
#------------------
# Help function (1.2.3): Update images on node added
#------------------
#' Update images on node added
#' 
#' Update images on node added.
#' @param cn A RODBC connection
#' @param dropImages Whether images should simply be dropped
#' @param net The network
#' @keywords internal
#------------------
updateImagesOnNodeAdded=function(cn,dropImages,net) {
  if (dropImages) SqlQuery(cn,paste("DELETE FROM ",net$tableinfo$image_table," WHERE 1"))    
  else {
    # Add node to all images
    str=paste("SELECT * FROM ",net$tableinfo$image_table,sep="")
    df=SqlQuery(cn,str)
    if (nrow(df)>0){
      uniqueTimes=unique(df[,1]) 
      # Note that for (nextTime in uniqueTimes) does not work! Not sure why. 
      for (i in 1:length(uniqueTimes)) {   # This does work
        # NB: Minus 1 in string since zero indexed in db
        str<-paste(
          "INSERT INTO ",net$tableinfo$image_table," SELECT '",uniqueTimes[i],
          "',NodeIndex,Name,Type,NodeValues,Parents,ParentValues,Params1,Params2,Params3,Dimensions,Misc FROM ",
          net$tableinfo$dbinfo$nodes," WHERE network='",net$name,"' AND NodeIndex=",length(net$nodes)-1,sep="")      
        df=SqlQuery(cn,str)
      }      
    }
  }  
}
#------------------
# Help function (1.2.4): Update datamap in database
#------------------
#' Update datamap in database
#' 
#' Update datamap in database
#' @param cn A RODBC connection
#' @param net A Bayesian network object
#' @keywords internal
#------------------
updateDatamapInDB<-function(
  cn,
  net
) {
  datamap=encodeWSSS(net$tableinfo$map)
  str=paste("UPDATE ",net$tableinfo$dbinfo$networks,
            " SET datamap =  ",datamap," WHERE network = '",net$name,"'",sep="")
  SqlQuery(cn,str)  
}
#------------------
# Help function for Deep Update: Deep update of result tables - NOT IMPLEMENTED
#------------------
# Notes from previous function (with single result table), with some changes, follow
# updateResultTablesOnNodeAdded=function(cn,net) {
#   # We know it will be a new node at the end of the last table, if there is room.
#   
#   if (net$tableinfo$dbinfo$metaresults) 
#   # Get lists of names and values in different tables
#   names_and_values=getVariableNamesAndValuesInResultTables(cn,tableinfo)
#   masterNames=names_and_values[1]
#   masterValues=names_and_values[2]
# 
#   # 
#   
#   
#   ultimateNode=length(net$nodes)
#   penultimateNode=ultimateNode-1
#   penultimateValues=length(net$nodes[[penultimateNode]]$values)  
#   str=paste("ALTER TABLE ",net$resultsTable,sep="")
#   for (i in 1:length(net$nodes[[ultimateNode]]$values)) {
#     if (i>1) {
#       str=paste(str,",")
#     }
#     str=paste(str," ADD COLUMN v_",ultimateNode,"_",i," DOUBLE DEFAULT NULL AFTER ",sep="")
#     if (i==1) {
#       str=paste(str,"v_",penultimateNode,"_",penultimateValues,sep="")
#     } else {
#       str=paste(str,"v_",ultimateNode,"_",i-1,sep="")
#     }
#   }  
#   SqlQuery(cn,str)      
# }
#------------------
# Help function for Deep Update: Get names of variables in different data tables
#------------------
#' Get names and number of values of variables in data tables
#' 
#' Get names and number of values of variables in data tables
#' @param cn A RODBC connection
#' @param tableinfo A table information object
#' @return A list of two vectors, containing variable names and numbers of values
#' @keywords internal
#------------------
getVariableNamesAndValuesInDataTables=function(cn,tableinfo) {
  getVariableNamesAndValuesInMetaTables(cn,tableinfo$data_tables,tableinfo$dbinfo$metadata,"DataTable")
}
#------------------
# Help function for Deep Update: Get names of variables in different result tables - UNUSED
#------------------
#' Get names and number of values of variables in result tables
#' 
#' Get names and number of values of variables in result tables
#' @param cn A RODBC connection
#' @param tableinfo A table information object
#' @return A list of two vectors, containing variable names and numbers of values
#' @keywords internal
#------------------
getVariableNamesAndValuesInResultTables=function(cn,tableinfo) {
  getVariableNamesAndValuesInMetaTables(cn,tableinfo$result_tables,tableinfo$dbinfo$metaresults,"ResultTable")
}
#------------------
# Help function for Deep Update: Get names of variables in different meta tables
#------------------
#' Get names and number of values of variables in meta tables
#' 
#' Get names and number of values of variables in meta tables
#' @param cn A RODBC connection
#' @param tablelist List of table names (data or result) to look for
#' @param tablename Meta table to use 
#' @param colname Column in meta table specifying tables of interest 
#' @return A list of two vectors, containing variable names and numbers of values
#' @keywords internal
#------------------
getVariableNamesAndValuesInMetaTables=function(cn,tablelist,tablename,colname) {
  names=c()
  values=c()
  for (table in tablelist) {
    str=paste("SELECT NodeNames,NodeValues FROM ",tablename," WHERE ",colname,"='",table,"'",sep="")
    df=SqlQuery(cn,str)
    names=c(names,parseWSSS_asStringVector(df[1,1]))
    values=c(values,parseWSSS_asStringVector(df[1,2]))
  }
  list(names=names,values=values)
}
#---------------------



