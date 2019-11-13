#------------------
# Principle Function (1): Restore image
#------------------
#' Restore a network's image
#' 
#' Restore a network's image. NB Due to time stamp resolution in mySQL, storing
#' multiple images in the same second may cause undefined behavior. 
#' All external inference sessions should be stopped before restoring an image.
#' @param cn An open RODBC connection
#' @param net The network. The network must have a valid image table name 
#' in the imageTable field - so network tables must have been initialized.
#' @param timeFrom The time stamp of the image to restore
#' @return The restored network
#' @export
#------------------
restoreImage<-function(
  cn,
  net,
  timeFrom
){
  # Check no external inference sessions running.
  if(length(net$inf_manager$running_ids)>0)
    stop("Stop the external inference session associated with this network before restoring image.")

  # Reset autocommit
  RODBC::odbcSetAutoCommit(cn,TRUE)
  
  # Restore image
  restoreImage_internal(cn,net,timeFrom)

  # Load network
  net=getNetwork(cn,net$name,net$inf_manager$connection_string,net$inf_manager$path)
  
  # Reset autocommit
  RODBC::odbcSetAutoCommit(cn,TRUE)
  
  # Return network
  return (net)
}
#------------------

#------------------
# Major subcomponent (1.1&): Restore image in backend. 
#------------------
#' Restore a network's image
#' 
#' Restore a network's image. NB Due to time stamp resolution in mySQL, storing
#' multiple images in the same second may cause undefined behavior. The image is restored
#' and can be fetched using getNetwork. Note that inf_manager must be copied if retrieved
#' in this way.
#' @param cn An open RODBC connection
#' @param net The network. The network must have a valid image table name 
#' in the imageTable field - so network tables must have been initialized.
#' @param timeFrom The time stamp of the image to restore
#' @return The time of the restored image
#' @keywords internal
#------------------
restoreImage_internal<-function(
  cn,
  net,
  timeFrom
){  
  time=getClosestToTime(cn,net$tableinfo$image_table,timeFrom,"time",
                        prior=T)
  
  nodes=net$tableinfo$dbinfo$nodes
  str<-paste(
    'UPDATE ',nodes,
    ' , (  SELECT NodeIndex,Name,Type,NodeValues,Parents,ParentValues,
    Params1,Params2,Params3,Dimensions,Misc 
    FROM ',net$tableinfo$image_table,'
    WHERE time="',time,'" ) as image
    SET ',nodes,'.NodeIndex =image.NodeIndex,
    ',nodes,'.Name=image.Name,
    ',nodes,'.Type=image.Type,
    ',nodes,'.NodeValues=image.NodeValues,
    ',nodes,'.Parents=image.Parents,
    ',nodes,'.ParentValues=image.ParentValues,
    ',nodes,'.Params1=image.Params1,
    ',nodes,'.Params2=image.Params2,
    ',nodes,'.Params3=image.Params3,
    ',nodes,'.Dimensions=image.Dimensions,
    ',nodes,'.Misc=image.Misc
    WHERE ',nodes,'.network="',net$name,'" AND ',nodes,'.NodeIndex=image.NodeIndex',
    sep="")
  SqlQuery(cn,str) 
  
  # Return network
  return (time)
}
#------------------

