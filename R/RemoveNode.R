#--------------------------
# Principle Functionality (1): Remove node from network
#--------------------------
#' Remove node from network
#' 
#' Remove node from network. Only childless nodes can be
#' removed. To remove nodes with children, either remove 
#' edges into the node's children (if the children are 
#' Noisy-Or or Dirichlet) or remove the children.
#' If a layout matrix exists, the row corresponding to the
#' removed node will also be removed.  
#' NB: A deep update must be performed on a network with a DB backend 
#' @param net the current network
#' @param index The index of the node to remove.
#' @param cn A RODBC connection. Required if performing deep update.
#' @param deepUpdate Specifies whether a deep update should be performed.
#' @param dropImages Specifies if images should be dropped on a deepUpdate.
#' @return Network with node removed.
#' @export
#--------------------------
removeNode<-function(
  net,
  index,
  cn=NULL,
  deepUpdate=F,
  dropImages=F
) {
  # Validate connection present if deep update desired
  if (deepUpdate && is.null(cn)) stop ("Connection required for deep update.")
  
  # Make sure node has no children
  for (Node in net$nodes) 
    if (index %in% Node$parents) 
      stop("Cannot remove node that has children.")
  
  # Find number of values of node to be removed (used in deep update if required)
  oldValues=length(net$nodes[[index]]$values)
  
  # Perform shallow update
  net=shallowUpdateOnNodeRemoved(net,index)
  
  # Perform deep update if desired
  if (deepUpdate) net=deepUpdateOnNodeRemoved(cn,net,index,oldValues,dropImages)
  
  # Return net
  return (net)
}

#--------------------------
# Major Sub-components (1.1): Shallow Updating - Remove node to R Bayesnet object
#--------------------------
#' Remove node from network
#' 
#' Remove node from R Bayesnet object
#' @param net the current network
#' @param index The index of the node to remove.
#' @return Network with node removed.
#' @keywords internal
#--------------------------
shallowUpdateOnNodeRemoved=function(
  net,
  index
) {
  # Remove node from layout
  if (!is.null(net$layout)) 
    net$layout=net$layout[-index,]
  
  # Remove node from node list
  net$nodes<-net$nodes[-index]
  
  # Adjust indices of remaining nodes
  if (index<=length(net$nodes)) 
    for (i in index:length(net$nodes)) 
      net$nodes[[i]]$index=net$nodes[[i]]$index-1
  
  # Adjust parent indices for remaining nodes who have parents with higher indices
  for (i in 1:length(net$nodes))
    net$nodes[[i]]$parents[which(net$nodes[[i]]$parents>index)]=
      net$nodes[[i]]$parents[which(net$nodes[[i]]$parents>index)]-1
    
  # Return updated network 
  return(net)
}
#--------------------------
# Major Sub-components (1.2): Deep Updating: - Removing node to DB
#--------------------------
#' Perform a deep update after removing a node.
#' 
#' Update a network in the database after a nodeis removed.
#' Note that updating images is permitted only when the removed node has no children 
#' in the network or in any network images.
#' Use deepUnpdateOnEdgeRemoved to remove egdes in images.
#' NB This function requires that there not exist multiple multiple tables with the same name as the master data
#' table in different data bases / data schema of the data management system.
#' @param cn An open RODBC connection.
#' @param net The network.
#' @param oldIndex The index of the node that has been removed.
#' @param oldValues The number of values the dropped node had.
#' @param dropImages Whether images should be dropped or updated. 
#' @param zeroIndexed Whether the oldIndex is zero or one indexed.
#' @return The updated network
#' @keywords internal
#--------------------------
deepUpdateOnNodeRemoved <- function (
  cn,
  net,
  oldIndex,
  oldValues,
  dropImages=F
) {  
  # Adjust for zero indexing
  oldIndex=oldIndex-1
  
  # Set autocommit
  RODBC::odbcSetAutoCommit(cn,FALSE)    
  
  # Update datamap
  net$tableinfo$map=net$tableinfo$map[-(oldIndex+1)] # Add 1 since it is zero indexed for sure now.
  updateDatamapInDB(cn,net)
  
  # Update nodes in network
  updateSerializedNetwork(cn,net,T)
  
  # Update Images
  updateImagesOnNodeRemoved(cn,dropImages,oldIndex,net)
    
  # Data tables are not updated
  
  # Recreate data views
  net=recreateDataView(cn,net,T)
  
  # Recreate results tables
  net=recreateResultTables(cn,net,T)
  
  # Reset autocommit
  RODBC::odbcSetAutoCommit(cn,TRUE)
 
  # Return network
  return(net)
}
#--------------------------


#--------------------------
# Help function for deep update (1.2.1): Update images on node removed
#--------------------------
#' Update images on node removed
#' 
#' Update images on node removed
#' @param cn A RODBC connection
#' @param dropImages Whether images should be dropped or altered
#' @param oldIndex Index of node that was removed
#' @param net The Bayesnet object
#' @keywords internal
#--------------------------
updateImagesOnNodeRemoved=function(
  cn,
  dropImages,
  oldIndex,
  net
  ){
  # Update network images
  # NB Primary Key of images table is Time and NodeIndex
  #   Remove node from all images
  if (dropImages) SqlQuery(cn,paste("DELETE FROM ",net$tableinfo$image_table," WHERE 1"))    
  else {
    str=paste("DELETE FROM ",net$tableinfo$image_table," WHERE NodeIndex=",oldIndex,sep="")
    SqlQuery(cn,str)
    
    # Adjust parent lists, and parent values of nodes
    str=paste("SELECT * FROM ",net$tableinfo$image_table,sep="")
    df=SqlQuery(cn,str)
    if (nrow(df)>0){
      for (i in 1:nrow(df)) {      
        # Remove removed node from parent lists and reduce indices of parents 
        # when these are larger than removed node.
        parents=parseWSSS_asIntegerVector(df[i,6])
        if (any(parents>oldIndex)) { # Note that everything is zero indexed & node has no children
          parents[which(parents>oldIndex)]=parents[which(parents>oldIndex)]-1
          newParents=encodeWSSS(parents)    
          str=paste("UPDATE ",net$tableinfo$image_table," SET parents=",newParents,
                    " WHERE Time='",df[i,1],"' AND NodeIndex=",df[i,2],sep="")
          SqlQuery(cn,str)      
        }
      }      
    }
    
    #   Reduce indices of nodes when these are larger than removed node.
    str=paste("UPDATE ",net$tableinfo$image_table," SET NodeIndex=NodeIndex-1 WHERE NodeIndex>",oldIndex)
    SqlQuery(cn,str)      
  }    
}
#--------------------------