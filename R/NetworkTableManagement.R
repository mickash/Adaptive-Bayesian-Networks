#-----------------
# Primary Function (1): Serialize Network
#-----------------
#' Create required entries and tables for a network at a 
#' location using default names.
#' 
#' Create required entries and tables for a network. Names are as specified in the network's tableinfo
#' object. The network and it's associated table names will be entered
#' into the location's network table. The nodes of the network
#' will be entered into the location's nodes table.
#' Where the network has the name "net", the the data (input) tables are called "net_data_table_x", 
#' the data (input) views are called "net_data_view_x", the results table is called "net_results", 
#' and the images table is called "net_images".
#' An initial image will be stored in the image table.
#' @param cn An open RODBC connection.
#' @param net The network.
#' @return The updated network
#' @export
#-----------------
serializeNetwork <- function (
  cn,
  net
) {
  # Copy tableinfo for ease
  tableinfo=net$tableinfo
  
  # Set autocommit
  RODBC::odbcSetAutoCommit(cn,FALSE)    
  
  ### Data tables ###
  # Create data tables if required
  if (length(tableinfo$data_tables)==0) {
    tableinfo$data_tables=createDataTables(cn,tableinfo,net)  
    # Fill in data in datamap
    tableinfo$map=seq(1,length(net$nodes))
    # Add data family name to tableinfo
    tableinfo$data_family=tableinfo$data_table_base
  }
  # Create views
  tableinfo$data_views=createDataViews(cn,tableinfo)
  
  ### Results Tables ###
  # Create result tables
  tableinfo$result_tables=createResultTables(cn,net,tableinfo)
  
  ### Images Table ###
  # Create image table
  tableinfo$image_table=createImageTable(cn,tableinfo)
  
  # Save network information
  addInformationToNetworks(cn,tableinfo,net$layout)
  
  # Serialize Nodes
  lapply(net$nodes,serializeNodes,net$name,tableinfo$dbinfo$nodes,cn)
  
  # Overwrite tableinfo
  net$tableinfo=tableinfo
  
  # Store Image (Avoids bad fail of SqlQuery regarding time on empty image table)
  storeImage(cn,net)
  
  # Reset autocommit
  RODBC::odbcSetAutoCommit(cn,TRUE)
    
  # Return network
  return (net)
}
#-----------------
# Primary Function (2): Unserialize network
#-----------------
#' Unserialize a network
#' 
#' Remove network entries from networks and nodes tables at a 
#' location, and remove all entries from default tables 
#' for a network at a location. The tables must have been 
#' created by defaultInitializeNetwork.
#' @param cn An open RODBC connection.
#' @param net The network.
#' @param disassociateMasterData Whether the network should no longer be associated
#' with its current master data tables.
#' @param dropMasterData Specifies whether the master data tables will also be dropped. 
#' If T but other networks still using the master data tables, then they will not be 
#' dropped and a warning will be given. A warning will also be given 
#' if the master data tables are not dropped when no other networks are using them. 
#' This can only be TRUE if disassociateMasterData is also TRUE
#' @return The network, with updated table names.
#' @export
#-----------------
unserializeNetwork<-function(
  cn,
  net,
  disassociateMasterData,
  dropMasterData
) {
  if (dropMasterData && !disassociateMasterData)
    stop ("Master data tables can only be dropped if also disassociated from network.")
  
  # Set autocommit
  RODBC::odbcSetAutoCommit(cn,FALSE)
  
  # Drop master data tables
  num=numberOfNetworksUsingDataTable(cn,net$tableinfo)
  if (dropMasterData) {
    if (num>1)
      warning("The specified master data table is used by other networks so cannot be dropped.")
    else {
      for (table in net$tableinfo$data_tables) {
        # Drop data tables
        SqlQuery(cn,paste("DROP TABLE IF EXISTS ",table,sep=""))            
        # Remove information about data tables from metadata
        SqlQuery(cn,paste("DELETE FROM ",net$tableinfo$metadata,
                          " WHERE DataTable='",table,"'",sep=""))          
      }
    }
  }
  else if (num==1)
    warning("The specified master data table has not been dropped, but is now used by no networks.")
  
  # Disassociate Master Data Tables
  if (disassociateMasterData) {
    net$tableinfo$data_family=NULL
    net$tableinfo$data_tables=c()    
    net$tableinfo$map=NULL
  }
  
  # Drop other tables and views
  for (view in net$tableinfo$data_views) 
    SqlQuery(cn,paste("DROP VIEW ",view,sep=""))  
  for (table in net$tableinfo$result_tables) {
    SqlQuery(cn,paste("DROP TABLE ",table,sep=""))
    # Remove information about result tables from metaresults
    SqlQuery(cn,paste("DELETE FROM ",net$tableinfo$dbinfo$metaresults,
                      " WHERE ResultTable='",table,"'",sep=""))   
  }
  SqlQuery(cn,paste("DROP TABLE ",net$tableinfo$image_table,sep=""))  
  SqlQuery(cn,paste("DELETE FROM ",net$tableinfo$dbinfo$nodes,
                    " WHERE network='",net$name,"'",sep=""))  
  SqlQuery(cn,paste("DELETE FROM ",net$tableinfo$dbinfo$networks,
                    " WHERE network='",net$name,"'",sep=""))  
  
  # Update tableinfo
  net$tableinfo$data_view=c()
  net$tableinfo$result_tables=c()
  net$tableinfo$image_table=NULL
  
  # Set autocommit
  RODBC::odbcSetAutoCommit(cn,TRUE)
  
  # Return network
  return (net)
}
#-----------------
# Private semi-principle function (3): Recreate views of data tables for a network
#-----------------
#' Recreate views of data tables for a network
#' 
#' Drops the existing views and then recreates them
#' @param cn An open RODBC connection.
#' @param net The network.
#' @param auto_commit_handled_elsewhere If calling function is handling transaction
#' @return The updated network
#' @keywords internal
#-----------------
recreateDataView<-function (
  cn,
  net,
  auto_commit_handled_elsewhere
) {
  # Set autocommit
  if (!auto_commit_handled_elsewhere) RODBC::odbcSetAutoCommit(cn,FALSE)    
  
  # Remove old views
  for (view in net$tableinfo$data_views) {
    SqlQuery(cn,paste("DROP VIEW",view))    
  }
  
  # Create new views
  net$tableinfo$data_views=createDataViews(cn,net$tableinfo)
  
  # Reset autocommit
  if (!auto_commit_handled_elsewhere) RODBC::odbcSetAutoCommit(cn,TRUE)    
  
  # Return updated network
  return (net)
}
#-----------------
# Private semi-principle function (4): Recreate result tables for a network
#-----------------
#' Recreate result tables for a network
#' 
#' Drops the existing result tables and then recreates them
#' @param cn An open RODBC connection.
#' @param net The network.
#' @param auto_commit_handled_elsewhere If calling function is handling transaction
#' @return The updated network
#' @keywords internal
#-----------------
recreateResultTables<-function (
  cn,
  net,
  auto_commit_handled_elsewhere
) {
  # Set autocommit
  if (!auto_commit_handled_elsewhere) RODBC::odbcSetAutoCommit(cn,FALSE)    
  
  # Remove old tables
  for (table in net$tableinfo$result_tables) {
    SqlQuery(cn,paste("DROP TABLE",table))    
    SqlQuery(cn,paste("DELETE FROM ",net$tableinfo$dbinfo$metaresults,
                      " WHERE ResultTable='",table,"'",sep=""))
  }
  
  # Create new tables
  net$tableinfo$result_tables=createResultTables(cn,net,net$tableinfo)
  
  # Reset autocommit
  if (!auto_commit_handled_elsewhere) RODBC::odbcSetAutoCommit(cn,TRUE)    
  
  # Return updated network
  return (net)
}
#-----------------
# Principle Function (5): Clear result tables
#-----------------
#' Remove all entries from result tables for a network.
#' 
#' Remove all entries from result tables for a network.
#' @param cn An open RODBC connection.
#' @param net The network.
#' @export
#-----------------
clearAllResults=function(
  cn,
  net
  ) {
  RODBC::odbcSetAutoCommit(cn,FALSE)
  for (table in net$tableinfo$result_tables) 
    SqlQuery(cn,paste("DELETE FROM",table,"WHERE 1"))  
  RODBC::odbcSetAutoCommit(cn,TRUE)  
}
#-----------------
# Principle Function (6): Clear image table
#-----------------
#' Remove all entries from image table for a network.
#' 
#' Remove all entries from image table for a network.
#' @param cn An open RODBC connection.
#' @param net The network.
#' @export
#-----------------
clearAllImages=function(
  cn,
  net
  ) {
  SqlQuery(cn,paste("DELETE FROM",net$tableinfo$image_table,"WHERE 1"))    
}
#-----------------
# Primary Function (7): Clear data tables
#-----------------
#' Remove all entries from data tables for a network.
#' 
#' Remove all entries from data tables for a network. If multiple networks are using the data tables the
#' function will fail with a message unless ignore_warnings is set to TRUE.
#' @param cn An open RODBC connection.
#' @param net The network.
#' @export
#-----------------
clearAllData=function(
  cn,
  net,
  ignore_warnings=F
  ) {
  if (numberOfNetworksUsingDataTable(cn,net$tableinfo)>1) {
    if (!ignore_warnings) {
      stop("The specified master data table is used by other networks. It has not been cleared.")      
    }
  }
  RODBC::odbcSetAutoCommit(cn,FALSE)
  for (table in net$tableinfo$data_tables) 
    SqlQuery(cn,paste("DELETE FROM",table,"WHERE 1"))  
  RODBC::odbcSetAutoCommit(cn,TRUE)  
}
#-----------------
# Primary Function (8): Update layout
#-----------------
#' Update serialized network's layout
#' 
#' Updates serialized network's layout.
#' @param cn An open RODBC connection.
#' @param net The network.
#' @param netsTable The name of the network table 
#' in the database.
#' @export
#-----------------
updateNetworkLayout <- function (
  cn,
  net
) {
  SqlQuery(cn,paste(
    "UPDATE ",net$tableinfo$dbinfo$networks," SET Layout=",
    encodeWSSS(net$layout)," WHERE network='",net$name,"'",sep=""))  
}
#-----------------
# Primary Function (9): Restore a network and overwrite another
#-----------------
#' Restore a network and overwrite another
#' 
#' This function does the following: 
#' (1) The network to be overwritten is loaded and unserialized, without
#' dropping the master data tables. 
#' (2) The network to be restored is loaded and cloned, with the cloned network having the network to be 
#' overwritten's name. 
#' (3) The cloned network is serialized. 
#' (4) The cloned network is returned.
#' @param cn A valid ODBC connection.
#' @param overwrite The name of the network to be overwritten.
#' @param restore The name of the network to be restored.
#' @return A clone of the restored network, with the overwritten network's name.
#' @export 
#-----------------
restoreAndOverwrite<-function(
  cn,
  overwrite,
  restore,
  connection_string=NULL,
  external_path=NULL,
  dbinfo=dbmsinfo()
) {
  oldNet=getNetwork(cn,overwrite)
  unserializeNetwork(cn,oldNet,F,F)
  newNet=getNetwork(cn,restore,connection_string,external_path,dbinfo)
  clonedNet=cloneBayesnet(newNet,oldNet$name)
  clonedNet=serializeNetwork(cn,clonedNet)
  return (clonedNet)
}
#-----------------



#-----------------
# Help function (1.1): Add information to Networks table
#-----------------
#' Add network information to networks table
#' 
#' Add network information to networks table
#' @param cn A RODBC Connection
#' @param tableinfo A table information object for the network
#' @param layout The layout matrix for the network
#' @keywords internal
#-----------------
addInformationToNetworks=function(
  cn,
  tableinfo,
  layout
  ) {
  str<-paste("INSERT INTO ",tableinfo$dbinfo$networks," VALUES ('",
             tableinfo$netname,"', '",
             tableinfo$data_family,"', ",
             encodeWSSS(tableinfo$data_views),", ",
             encodeWSSS(tableinfo$result_tables),", '",
             tableinfo$image_table,"', ",
             encodeWSSS(tableinfo$map),", ",
             encodeWSSS(layout),", ",
             tableinfo$table_length,")"
             ,sep="")
  SqlQuery(cn,str)  
}
#-----------------
# Help function (1.2): Create Data tables
#-----------------
#' Create Data tables
#' 
#' Create Data tables
#' @param cn A RODBC Connection
#' @param tableinfo Table information object for network
#' @param net The Bayesnet object
#' @return Vector of data table names
#' @keywords internal
#-----------------
createDataTables=function(cn,tableinfo,net) {
  # NOTE: net can have old tableinfo. Do not look at it. Use net only for nodes. Use passed tableinfo object.
  data_table_names=c()
  name_base=tableinfo$data_table_base
  table_length=tableinfo$table_length
  number_of_nodes=length(net$nodes)

  # Add data family name to tableinfo, for adding info to metadata
  # Note that this is temporary, since the tableinfo is not returned.
  # So this is done again in the serializeNetwork function.
  tableinfo$data_family=tableinfo$data_table_base
  
  # Find number of full tables required.
  full_tables_required = number_of_nodes %/% table_length
  # Find number of nodes in final partial table (can be zero).
  size_of_partial_table = number_of_nodes %% table_length
  # Create full tables
  if (full_tables_required>0) {
    for (i in 1:full_tables_required) {
      table_name=createDataTable(cn,name_base,i,table_length,table_length)
      data_table_names=c(data_table_names,table_name)      
      addInformationToMetaData(cn,tableinfo$dbinfo$metadata,tableinfo,net,
                                table_name,i,table_length,table_length)
    }
  }
  # Create partial table
  if (size_of_partial_table>0) {
    table_name=createDataTable(cn,name_base,full_tables_required+1,size_of_partial_table,table_length)
    data_table_names=c(data_table_names,table_name)
    addInformationToMetaData(cn,tableinfo$dbinfo$metadata,tableinfo,net,table_name,full_tables_required+1,size_of_partial_table,table_length)
  }    
  
  # Return names
  return (data_table_names)
}
#-----------------
# Help function (1.2.1): Create Data table (singular)
#-----------------
#' Create Data table 
#' 
#' Create Data table (singular).
#' @param cn A RODBC Connection
#' @param name_base Base name for the data tables
#' @param table_number Number of data table
#' @param number_of_nodes The number of nodes in the data table
#' @param max_length_of_table The maximum number of nodes in the table
#' @return The data table name
#' @keywords internal
#-----------------
createDataTable=function(
  cn,
  name_base,
  table_number,
  number_of_nodes,
  max_length_of_table
  ) {
  table_name=paste(name_base,table_number,sep="_")
  base=(table_number-1)*max_length_of_table
  # Create Query String
  str<-paste("CREATE TABLE ",table_name," (
             item int NOT NULL AUTO_INCREMENT PRIMARY KEY",sep="")      
  for (j in 1:number_of_nodes) {
    str<-paste(str,", v_",base+j," INT DEFAULT -1",sep="")
  }
  str<-paste(str,", Time TIMESTAMP DEFAULT CURRENT_TIMESTAMP)")
  # Execute Query
  SqlQuery(cn,str)
  # Return name
  return (table_name)
}
#-----------------
# Help function (1.2.2): Add information to metadata table
#-----------------
#' Add information to metadata table
#' 
#' Add information to metadata table
#' @param cn A RODBC connection
#' @param metatable The metatable to add information to
#' @param tableinfo Table information object for network, must have valid data family name.
#' @param net The Bayesnet object
#' @param table_name The name of the table information is being added about
#' @param table_number Number of the result table
#' @param number_of_nodes The number of nodes in the result table
#' @param max_length_of_table The maximum number of nodes in the result table
#' @keywords internal
#-----------------
addInformationToMetaData=function(
  cn,
  metatable,
  tableinfo,
  net,
  table_name,
  table_number,
  number_of_nodes,
  max_length_of_table
) {
  # NOTE: net can have old tableinfo. Use passed tableinfo object.
  first=(table_number-1)*max_length_of_table
  last=first+number_of_nodes-1
  name <- function (object) object$name
  value <- function (object) length(object$values)
  encodedNames=encodeWSSS(sapply(net$nodes[first:last],name))
  encodedValues=encodeWSSS(sapply(net$nodes[first:last],value))
  str=paste("INSERT INTO ",metatable,
            " VALUES ('",table_name,"','",tableinfo$data_family,"',",encodedNames,",",encodedValues,",",number_of_nodes,")",
            sep="")
  SqlQuery(cn,str)      
}
#-----------------
# Help function (1.3): Create Data views
#-----------------
#' Create Data views
#' 
#' Create Data views
#' @param cn A RODBC Connection
#' @param tableinfo Table information object for network
#' @return Vector of data view names
#' @keywords internal
#-----------------
createDataViews<-function (
  cn,
  tableinfo
  ) {
  view_names=c()
  name_base=tableinfo$data_table_base
  table_length=tableinfo$table_length
  number_of_nodes=length(tableinfo$map)
  # Find number of full views required.
  full_tables_required = number_of_nodes %/% table_length
  # Find number of nodes in final partial view (can be zero).
  size_of_partial_table = number_of_nodes %% table_length
  # Create full views
  if (full_tables_required>0) {
    for (i in 1:full_tables_required) {
      view_name=createDataView(cn,tableinfo$data_tables,tableinfo$map,
                               name_base,i,table_length,table_length)
      view_names=c(view_names,view_name)
    }
  }
  # Create partial table
  if (size_of_partial_table>0) {
    view_name=createDataView(cn,tableinfo$data_tables,tableinfo$map,
                             name_base,full_tables_required+1,size_of_partial_table,table_length)
    view_names=c(view_names,view_name)
  }
  return (view_names)
}
#-----------------
# Help function (1.3.1): Create Data view (singular)
#-----------------
#' Create Data view
#' 
#' Create Data view (singular).
#' @param cn A RODBC Connection
#' @param datatables The names of the data tables associated with the network
#' @param datamap Network's datamap
#' @param name_base Base name for the data views
#' @param table_number Number of the data view
#' @param number_of_nodes The number of nodes in the data view
#' @param max_length_of_table The maximum number of nodes in the view
#' @return The data view name
#' @keywords internal
#-----------------
createDataView=function(
  cn,
  datatables,
  datamap,
  name_base,
  table_number,
  number_of_nodes,
  max_length_of_table
  ) {
  view_name=paste(name_base,"view",table_number,sep="_")
  base=(table_number-1)*max_length_of_table
  # Create Query String
  str=paste("create view",view_name,"as select",paste(datatables[1],'.','item as item',sep=""))
  for (j in 1:number_of_nodes) str=paste(str," , v_",datamap[base+j]," as v_",base+j,sep="")
  str=paste(str,",",paste(datatables[1],'.','Time as Time',sep=""),"from",paste(datatables,collapse=','))
  # Execute Query
  SqlQuery(cn,str)
  # Return name
  return (view_name)
}
#-----------------
# Help function (1.4): Create Result tables
#-----------------
#' Create Result tables
#' 
#' Create Result tables
#' @param cn A RODBC Connection
#' @param net The Bayesnet object (perhaps with out of date tableinfo)
#' @param tableinfo Table information object for network 
#' @return Vector of result table names
#' @keywords internal
#-----------------
createResultTables <- function (
  cn,
  net,
  tableinfo
  ) {
  result_table_names=c()
  name_base=tableinfo$result_table_base
  table_length=tableinfo$table_length
  number_of_nodes=length(tableinfo$map)
  # Find number of full tables required.
  full_tables_required = number_of_nodes %/% table_length
  # Find number of nodes in final partial table (can be zero).
  size_of_partial_table = number_of_nodes %% table_length
  # Create full tables
  if (full_tables_required>0) {
    for (i in 1:full_tables_required) {
      table_name=createResultTable(cn,name_base,i,net,table_length)
      result_table_names=c(result_table_names,table_name)
      addInformationToMetaResults(cn,tableinfo$dbinfo$metaresults,tableinfo,net,table_name,i,table_length,table_length)
    }
  }
  # Create partial table
  if (size_of_partial_table>0) {
    table_name=createResultTable(cn,name_base,full_tables_required+1,net,table_length)
    result_table_names=c(result_table_names,table_name)
    addInformationToMetaResults(cn,tableinfo$dbinfo$metaresults,tableinfo,net,table_name,full_tables_required+1,size_of_partial_table,table_length)
  }
  return (result_table_names)
} 
#-----------------
# Help function (1.4.1): Create Result table (singular)
#-----------------
#' Create Result table
#' 
#' Create Result table (singular).
#' @param cn A RODBC Connection
#' @param name_base Base name for the result table
#' @param table_number Number of the result table
#' @param net The Bayesnet object
#' @param max_length_of_table The maximum number of nodes in the result table
#' @return The result table name
#' @keywords internal
#-----------------
createResultTable=function(
  cn,
  name_base,
  table_number,
  net,
  max_length_of_table
  ) {
  table_name=paste(name_base,table_number,sep="_")
  base=(table_number-1)*max_length_of_table
  # Create Query String
  str<-paste("CREATE TABLE ",table_name," (
             item int NOT NULL PRIMARY KEY",sep="")
  lim=min(base+max_length_of_table,length(net$nodes))
  for (i in (base+1):lim) {
    for (j in 1:length(net$nodes[[base+i]]$values)) {
      str<-paste(str,", v_",i,"_",j," DOUBLE DEFAULT NULL",sep="")      
    }
  }
  str<-paste(str,", Time TIMESTAMP DEFAULT CURRENT_TIMESTAMP)")
  # Execute Query
  SqlQuery(cn,str)
  # Return name
  return (table_name)
}
#-----------------
# Help function (1.4.2): Add information to metaresults table
#-----------------
#' Add information to metaresults table
#' 
#' Add information to metaresults table
#' @param cn A RODBC connection
#' @param metatable The metatable to add information to
#' @param net The Bayesnet object
#' @param table_name The name of the table information is being added about
#' @param table_number Number of the result table
#' @param number_of_nodes The number of nodes in the result table
#' @param max_length_of_table The maximum number of nodes in the result table
#' @keywords internal
#-----------------
addInformationToMetaResults=function(
  cn,
  metatable,
  tableinfo,
  net,
  table_name,
  table_number,
  number_of_nodes,
  max_length_of_table
  ) {
  # NOTE: net can have old tableinfo. Use passed tableinfo object.
  first=(table_number-1)*max_length_of_table
  last=first+number_of_nodes-1
  name <- function (object) object$name
  value <- function (object) length(object$values)
  encodedNames=encodeWSSS(sapply(net$nodes[first:last],name))
  encodedValues=encodeWSSS(sapply(net$nodes[first:last],value))
  str=paste("INSERT INTO ",metatable,
            " VALUES ('",table_name,"',",encodedNames,",",encodedValues,",",number_of_nodes,")",
            sep="")
  SqlQuery(cn,str)      
}
#-----------------
# Help function (1.5): Create image table
#-----------------
#' Create image table
#' 
#' Create image table
#' @param cn A RODBC Connection
#' @param tableinfo Table information object for network
#' @return Image table name
#' @keywords internal
#-----------------
createImageTable <- function (cn,tableinfo) {
  table=tableinfo$image_table_base
  str<-paste("CREATE TABLE ",table," (
             Time TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,             
             NodeIndex int NOT NULL,
             Name VARCHAR(64) NOT NULL,
             Type VARCHAR(64) NOT NULL,
             NodeValues VARCHAR(512) NOT NULL,
             Parents VARCHAR(512),
             ParentValues VARCHAR(512),
             Params1 VARCHAR(5000),
             Params2 VARCHAR(5000),
             Params3 VARCHAR(5000),
             Dimensions VARCHAR(32),
             Misc VARCHAR(5000),
             PRIMARY KEY(Time,NodeIndex))",
             sep="")
  SqlQuery(cn,str)
  return (table)
}
#-----------------
# Help function (7.1&): Find networks associated with a master data table.
#-----------------
#' Find networks associated with a master data table.
#' 
#' Find networks associated with a master data table.
#' @param cn An open RODBC connection.
#' @param dataTable The name of the master data table.
#' @param networks The name of the network table at the location.
#' @return A data frame giving the networks using the master data table and the nodes they exclude, in 
#' the form of a white space separated string of indices.
#' @keywords internal
#-----------------
networksUsingDataTable<-function(
  cn,
  tableinfo
) {
  # Assume first data table is sufficient.
  str=paste("SELECT network FROM ",tableinfo$dbinfo$networks,
            " WHERE DataFamily='",tableinfo$data_family,"'",sep="")
  df=SqlQuery(cn,str)
  return (df)
}
#-----------------
# Help function (6.2&): Find number of networks associated with a master data table.
#-----------------
#' Find number of networks associated with a master data table.
#' 
#' Find number of networks associated with a master data table.
#' @param cn An open RODBC connection.
#' @param dataTable The name of the master data table.
#' @param networks The name of the network table at the location.
#' @keywords internal
#-----------------
numberOfNetworksUsingDataTable<-function(
  cn,
  tableinfo
  ) {
  nrow(networksUsingDataTable(cn,tableinfo))
}
#-----------------






#' Update serialized network's nodes
#' 
#' Update serialized network's nodes. If nodes have been added or removed you must use the reserialize
#' function, not the updateSerializedNetwork function.
#' @param cn An open RODBC connection.
#' @param net The network.
#' @param nodesTable The name of the nodes table
#' @param networksTable The name of the networks table
#' @param autocommitManagedOutsideScope Set to T if you have are controlling the connection's autocommit
#' outside the scope of this function
#' @keywords internal
updateSerializedNetwork <- function (
  cn,
  net,
  autocommitManagedOutsideScope=F
) {
  # Set autocommit
  if (!autocommitManagedOutsideScope)  RODBC::odbcSetAutoCommit(cn,FALSE)    
  
  # Update nodes
  SqlQuery(cn,paste("DELETE FROM ",net$tableinfo$dbinfo$nodes,
                    " WHERE network='",net$name,"'",sep=""))  
  lapply(net$nodes,serializeNodes,net$name,net$tableinfo$dbinfo$nodes,cn)    
  
  # Reset autocommit
  if (!autocommitManagedOutsideScope)  RODBC::odbcSetAutoCommit(cn,TRUE)
}

getTableNumberColumns <- function (cn,table) {
  str=paste("SELECT COUNT(*) FROM INFORMATION_SCHEMA.COLUMNS WHERE table_name = '",table,"'",sep="")
  SqlQuery(cn,str)[1,1]
} 







#' Drop a network table
#' 
#' Drop a network table
#' @param connection An open RODBC connection
#' @param net The network
#' @param type Valid values are: "data", "results" and "images"
#' @return The network with the table name set to NULL
# dropNetworkTable <- function (connection,net,type) {
#   table=getNetworkTableName(type,net)
#   SqlQuery(
#     connection,
#     paste("DROP TABLE ",table,sep="")
#   )
#   return (setNetworkTableNameToNull(type,net))
# }
# 
#' Clear a network table
#' 
#' @param connection An open RODBC connection
#' @param net The network
#' @param type Valid values are: "data", "results" and "images"
# clearNetworkTable <- function (connection,net,type) {
#   table=getNetworkTableName(type,net)
#   SqlQuery(
#     connection,
#     paste("DELETE * FROM ",table,sep="")
#   )
# }
# getNetworkTableName <- function (type,net){
#   table=NULL
#   if (identical(type,"data")) {
#     table=net$dataTable
#   } else if (identical(type,"results")) {
#     table=net$resultsTable
#   } else if (identical(type,"images")) {
#     table=net$imagesTable
#   } else {
#     stop("Invalid argument: No such table.")
#   }
#   if (is.null(table)) {
#     stop(paste(
#       "Network does not have a specified ",type,
#       " table.",sep=""))
#   }
#   return (table)
# }
# setNetworkTableNameToNull <- function (type,net){
#   if (identical(type,"data")) {
#     net$dataTable<-NULL
#   } else if (identical(type,"results")) {
#     net$resultsTable<-NULL
#   } else if (identical(type,"images")) {
#     net$imagesTable<-NULL
#   } else {
#     stop("Invalid argument: No such table.")
#   }
#   return (net)
# }

