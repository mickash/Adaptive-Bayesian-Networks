#------------------------------
# Principle Function (1): Create inference management object
#------------------------------
#' Create inference management object
#' 
#' Create inference management object
#' @param name Network name
#' @param external_path Path to external inference executables
#' @param connection_string Connection string to database.
#' @return Inference management object
#' @export 
#------------------------------
inference_management=function(
  name,
  external_path=NULL,
  connection_string=NULL
  ) {
  out=list(
    path=external_path,
    connection_string=connection_string,
    base=prod(as.numeric(charToRaw(name))) %% 1000000, # Make a 'unique' number: Hash name
    step=0,
    running_ids=c()
  )
  class(out)="inf_manager"
  return (out)
}
#------------------------------
# Private semi-principle function (2): Clone inference management object
#------------------------------
#' Clone inference management object
#' 
#' Clone inference management object
#' @param new_name The name of the new, cloned network
#' @param other The inference management object to be cloned
#' @return A new inference management object
#' @keywords internal
#------------------------------
clone_inference_management=function(
  new_name,
  other
) {
  out=list(
    path=other$path,
    connection_string=other$connection_string,
    base=prod(as.numeric(charToRaw(new_name))) %% 1000000, # Make a 'unique' number: Hash name
    step=0,
    running_ids=c()
  )
  class(out)="inf_manage"
  return (out)
}
#------------------------------
