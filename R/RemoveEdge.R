#-----------------
# Primary Functionality: Remove edge into a node
#-----------------
#' Remove an edge into/a parent to a node
#' 
#' Remove an edge into/a parent to a node. The node must be Noisy-Or or Dirichlet. 
#' @param network The network
#' @param child The index of the node to remove the parent from.
#' @param parent The index of the node to remove as a parent.
#' @param params For a multinomial noisy-or node, this is The new parameter 2 matrix.
#' If NULL a weighted mutex matrix is created. For a dirichlet node, this is the new
#' parameters matrix.
#' @param cn A RODBC connection. Required if performing deep update.
#' @param deepUpdate Specifies whether a deep update should be performed.
#' @param dropImages Specifies if images should be dropped on a deepUpdate.
#' @return The updated network
#' @export
#-----------------
removeEdge=function(
  net,
  child,
  parent,
  params=NULL,
  cn=NULL,
  deepUpdate=F,
  dropImages=F
  ) {
  # Validate connection present if deep update desired
  if (deepUpdate && is.null(cn)) stop ("Connection required for deep update.")
  
  # Perform shallow update and check that node is suitable
  if (class(net$nodes[[child]])=='dirichlet')
    net$nodes[[child]]=removeEdgeDirichlet(net$nodes[[child]],parent,params)
  else if (class(net$nodes[[child]])=='noisyor')
    net=removeEdgeNoisyOr(net,child,parent,params)
  else 
    stop("Edges can only be removed from Noisy-Or and Dirichlet nodes.")
  
  # Perform deep update if required
  if (deepUpdate) net=deepUpdateOnEdgeRemoved(cn,net,parent,child,dropImages)
    
  # Return network
  return (net)
}
#-----------------

#-----------------
# Major Subcomponent: Remove edge into a dirichlet node
#-----------------
#' Remove an edge into/a parent to a dirichlet node
#' 
#' Remove an edge into/a parent to a dirichlet node
#' @param node The dirichlet node to remove the parent from.
#' @param parent The index of the node to remove as a parent.
#' @param params The new parameters for the child.
#' @return The updated node
#' @keywords internal
#-----------------
removeEdgeDirichlet <- function (node,parent,params){
  if (!any(node$parents==parent)) {
    stop("Parent not found.")
  }
  i<-which(node$parents==parent)
  if (ncol(params)!=ncol(node$params)) {
    stop("Invalid number of columns in parameter matrix.")
  }
  if (nrow(params)!=nrow(node$params)/node$pvals[i]) {
    stop("Invalid number of rows in parameter matrix.")
  }
  
  node$parents<-node$parents[-i]
  node$pvals<-node$pvals[-i]
  node$params<-params
  
  return (node)
}
#-----------------

#-----------------
# Major Subcomponent: Remove edge into a noisy- node
#-----------------
#' Remove an edge into/a parent to a noisy-or node
#' 
#' Remove an edge into/a parent to a noisy-or node
#' @param network The network
#' @param child The index of the noisy-or node to remove the parent from.
#' @param parent The index of the node to remove as a parent.
#' @param params2 The new parameter 2 matrix for multinomial noisy or nodes.
#' If NULL a weighted mutex matrix is created.
#' @return The updated network
#' @keywords internal
#-----------------
removeEdgeNoisyOr <- function (
  network,
  child,
  parent,
  params2=NULL
){
  node=network$nodes[[child]]
  if (!any(node$parents==parent)) {
    stop("Parent not found.")
  }
  
  i<-which(node$parents==parent)
  j<-node$pindices[i]
  additionalRows=node$pvals[i]-2
  
  k<-nrow(node$params)
  if (i!=length(node$parents)) {
    node$params[(j+additionalRows+1):nrow(node$params),1]=node$params[(j+additionalRows+1):nrow(node$params),1]-1
    node$pindices[(i+1):length(node$parents)]<-node$pindices[(i+1):length(node$parents)]-1
  }
  
  node$parents<-node$parents[-i]
  node$pindices<-node$pindices[-i]
  node$pvals<-node$pvals[-i]
  
  indices=j:(j+additionalRows)
  node$params<-matrix(node$params[-indices,],ncol=length(node$values)+1)
  
  if (length(node$values)!=2) {
    if (is.null(params2)) {
      params2<-makeWeightedMutex(length(node$parents),length(node$values))
    }
    if (
      (ncol(params2)!=length(node$values)) ||
        (nrow(params2)!=length(node$values)^length(node$pvals))    
    ) {
      stop('Invalid second parameter matrix.')    
    }
    node$params2<-params2
  } 
  network$nodes[[child]]=node
  return (network)
}
#-----------------

#-----------------
# Major Subcomponent: Perform deep update after removing an edge
#-----------------
#' Perform a deep update after removing an edge
#' 
#' Update the images and tables of a network after removing an edge.
#' Note that to be able to update the saved network images, the child must be 
#' a binary noisy or node. 
#' @param cn An open RODBC connection.
#' @param net The network.
#' @param from The index of the parent node
#' @param to The index of the child node
#' @param dropImages Whether images should be dropped or updated. 
#' @param zeroIndexed Whether the from and to arguments are 0 or 1 indexed.
#' @return The updated network
#' @keywords internal
#-----------------
deepUpdateOnEdgeRemoved<-function(
  cn,
  net,
  from,
  to,
  dropImages=F
){  
  #   a.  Remove edge in current network.
  #   b.  Check if child node is binary noisy-or.
  #     i.  If so, then in all images:
  #       1.	Remove edge
  #         a.	Remove parent from child’s parents list
  #         b.	Remove entry in child’s parent values list
  #         c.	Remove rows to child parameters corresponding to new parent.
  #     ii.	If not then drop all images
  
  # Adjust for zero indexing
  from=from-1
  to=to-1
  
  # Set autocommit
  RODBC::odbcSetAutoCommit(cn,FALSE)    
  
  # Update nodes in network
  updateSerializedNetwork(cn,net,T)
  
  # Drop or update images
  updateImagesOnEdgeRemoved(cn,net,dropImages,to,from)
    
  # No need to adjust data views or result tables 
  
  # Reset autocommit
  RODBC::odbcSetAutoCommit(cn,TRUE)      
  
  # Return updated network
  return(net)
}
#-----------------

#-----------------
# Help Function: Update images after removing an edge
#-----------------
#' Update images after removing an edge
#' 
#' Update the images after removing an edge.
#' Note that to be able to update the saved network images, the child must be 
#' a binary noisy-or node. 
#' @param cn An open RODBC connection.
#' @param net The network.
#' @param dropImages Whether images should be dropped or updated. 
#' @param to The index of the child node
#' @param from The index of the parent node
#' @keywords internal
#-----------------
updateImagesOnEdgeRemoved=function(
  cn,
  net,
  dropImages,
  to,
  from
  ){
  if (dropImages || class(net$nodes[[to+1]])!="noisyor" || length(net$nodes[[to+1]]$values)!=2) {
    if (!dropImages) 
      warning ("Updating old images only works when removing edges into binary noisyor nodes. Images have been dropped.")
    SqlQuery(cn,paste("DELETE FROM",net$imagesTable,"WHERE 1"))  
  } 
  else {
    # Update Images
    # Adjust parent lists, and parent Values and parameters of children
    str=paste("SELECT * FROM ",net$tableinfo$image_table," WHERE NodeIndex=",to,sep="")
    df=SqlQuery(cn,str)
    if (nrow(df)>0){
      for (i in 1:nrow(df)) {
        # Ajust parent values and parameters of children
        oldChild=unserializeNode(df[i,])# This should work. Col 1 is Time in images and Network in nodes.
        oldPIndex=which(oldChild$parents==from+1)
        oldPValue=oldChild$pvals[oldPIndex]
        parents=oldChild$parents[-oldPIndex]
        parentValues=oldChild$pvals[-oldPIndex]
        pindices=oldChild$pindices[-oldPIndex]
        if (length(pindices)>=oldPIndex){
          pindices[oldPIndex:length(pindices)]=pindices[oldPIndex:length(pindices)]-(oldPValue-1)
        }
        newParents=encodeWSSS(parents-1)    
        newParentValues=encodeWSSS(parentValues)
        newParams1=encodeWSSS(NULL)
        params2Matrix=NULL
        if (length(pindices)>0) {
          params2Matrix=matrix(pindices-1,nrow=1)
        }
        newParams2=encodeWSSS(params2Matrix)
        newParams3=encodeWSSS(NULL)
        #   Replace* children parameters with new parameters!
        params1=oldChild$params[-which(oldChild$params[,1]==oldPIndex),,drop=F]
        if (nrow(params1)>=oldChild$pindices[oldPIndex]){
          params1[oldChild$pindices[oldPIndex]:nrow(params1)]=params1[oldChild$pindices[oldPIndex]:nrow(params1)]-1
        }
        newParams1=encodeWSSS(params1)        
        newDimensions=encodeDimensions(params1,params2Matrix,NULL)
        # when these are larger than removed node.
        str=paste("UPDATE ",net$tableinfo$image_table," SET ",
                  "ParentValues=",newParentValues,
                  ",Params1=",newParams1,",Params2=",newParams2,",Params3=",newParams3,
                  ", parents=",newParents,",Dimensions=",newDimensions," WHERE Time='",df[i,1],
                  "' AND NodeIndex=",df[i,2],sep="")
        SqlQuery(cn,str)      
      }
    }
  }
}
#-----------------
