#-----------------------
# Principle Function (1): Store image
#-----------------------
#' Store a network's image
#' 
#' Store a network's image. NB Due to time stamp resolution in mySQL, storing
#' multiple images in the same second may cause undefined behavior.
#' @param cn An open RODBC connection
#' @param net The network. The network must have a valid image table name 
#' in the imageTable field - so network tables must have been initialized.
#' @export
#-----------------------
storeImage<-function(
  cn,
  net
  ){
  str<-paste('INSERT ',net$tableinfo$image_table,
             ' (NodeIndex,Name,Type,NodeValues,Parents,ParentValues,
             Params1,Params2,Params3,Dimensions,Misc) 
             SELECT NodeIndex,Name,Type,NodeValues,Parents,ParentValues,
             Params1,Params2,Params3,Dimensions,Misc 
             FROM ',net$tableinfo$dbinfo$nodes,' WHERE network="',net$name,'"',sep="")
  SqlQuery(cn,str)  
}
#-----------------------
