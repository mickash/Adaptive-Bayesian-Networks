#----------------
# Principle Function (1):  Start external inference
#----------------
#' Start external inference
#' 
#' Start external inference. Only one external inference engine session can run per
#' network. 
#' @param net The network.
#' @param Number The number of samples per inference case
#' @param NextItem The first item to start from. (0-Find Latest)
#' @param Update Whether network parameters should be updated.
#' @param AssumeIntegrity Specifies if engine can assume no other processes will change
#' database records while running. We recommend you set this to TRUE and act accordingly.
#' @param StoreImages Whether images should be stored.
#' @param Compression_Setting 'None', 'Until' or 'All'. None means compression value 2 is
#' used through out. All means compression value 1 is used through out. Until will use
#' value 1 until the process catches up with the number of entries in the data table, and
#' value 2 after that.
#' @param Compression_Value1 The number of items to process during an initial catch up
#' @param Compression_Value2 The number of items to process after any initial catch up
#' @param EIE_pause The duration in milliseconds that the external inference engine 
#' will wait before rechecking if new data has appeared in the data table.
#' @param DBMS_Timeout Currently unused
#' @param pause The duration in seconds to wait after making system call
#' @return Updated network. 
#' @export
#----------------
startExternalInference=function (
  net,
  number=1000,
  nextItem=0,
  Update = TRUE,
  StoreImages = TRUE,
  AssumeIntegrity = TRUE,
  Compression_Setting = 'None',
  Compression_Value1 = 10,
  Compression_Value2 = 1,
  EIE_pause=500,
  DBMS_Timeout = 1000,
  pause=2
  ) {
  # Test that path is present
  if (is.null(net$inf_manager$path) || nchar(net$inf_manager$path)==0) 
    stop("No path for external inference engine executable has been specified")

  # Test that connection string is present
  if (is.null(net$inf_manager$connection_string) || nchar(net$inf_manager$connection_string)==0) 
    stop("No path for external inference engine executable has been specified")
  
  # Test no other inference engine being run by network
  if (length(net$inf_manager$running_ids)>0) 
    stop("Cannot run multiple external inference engines for a single network.")

  # Calculate System ID
  SystemID=net$inf_manager$base+net$inf_manager$step
  
  # Create settings
  settings=makeExternalInferenceSettings(
    net,
    SystemID,
    number,
    nextItem,
    Update,
    AssumeIntegrity, #AssumeIntegrity. TRUE since we limit to one engine per network.
    StoreImages,
    Compression_Setting,
    Compression_Value1,
    Compression_Value2,
    EIE_pause,
    DBMS_Timeout
    )     
  # Create command line from settings
  commandLine=makeCommandArguments(settings)
  # Run external inference engine
  system2(
    paste(net$inf_manager$path,"\\App.exe",sep=""),
    args=commandLine,
    wait=FALSE)
  # Pause to let process set up and start executing
  Sys.sleep(pause)
  # Record new inference application being run
  net$inf_manager$step=net$inf_manager$step+1  
  net$inf_manager$running_ids=c(net$inf_manager$running_ids,SystemID)
  # Return network
  return (net)
}
#----------------
# Principle Function (2): Stop external inference engine
#----------------
#' Stop the external inference engine.
#' 
#' Stop the external inference engine.
#' @param settings A valid externalInferenceSettings object
#' @param pause The duration in seconds to wait after making system call
#' @return Updated network. 
#' @export
#----------------
stopAutomaticInference <- function (
  net,
  pause=2
) {
  # Test that there is a current inference engine
  if (length(net$inf_manager$running_ids[1])==0) 
    stop ("No current external inference engine session for this network.")
  # Stop current inference engine
  system2(
    paste(net$inf_manager$path,"\\Terminator.exe",sep=""),
    args=net$inf_manager$running_ids[1]
  )    
  # Pause to let process set up and start executing
  Sys.sleep(pause)
  # Get rid of record of session
  net$inf_manager$running_ids=c()
  # Return network
  return (net)
}
#----------------
# Principle Function (3): Run external MI test
#----------------
#' Run the external MI test
#' 
#' Run the external MI test on a single item passed as evidence
#' @param net A network object
#' @param evidence The evidence, in 1-indexed, NA=missing form.
#' @param nodes The nodes to check mutual information of environment 
#' variables on, in 1-indexed form.
#' @param envNodes The environment variables, in 1-indexed form.
#' @param weights The weights, if a combined MI result desired. Should be one 
#' weight for each node, or NULL. Intended to be probabilities of nodes,
#' though importance weights could be used as well.
#' @param number The number of samples
#' @param filename The full filename to write the results to.
#' @param appPath The path to the EstimateMI.exe
#' @param pause The duration in seconds to wait after making system call
#' @return Nothing. But writes the MI matrix nodes vs envNodes. If a weights 
#' vector has been given, there will be an additional row which
#' is a weighted combination of the other rows.
#' @export
#----------------
performMI<-function(
  net,
  evidence,
  nodes,
  envNodes,
  weights,
  number,
  filename,
  pause=2
) {
  # Check evidence vector same length as nodes
  if (length(evidence)!=length(net$nodes)) stop('Evidence/node length mismatch.')
  
  # Adjust for zero-index and and -1 instead of NA
  evidence=evidence-1
  evidence[which(is.na(evidence))]=-1      
  nodes=nodes-1      
  envNodes=envNodes-1
  
  # Set up weights string
  weightsString=""
  if (!is.null(weights)) weightsString=paste(weights,collapse=" ")
  
  # Create command line
  cmdLine<-paste(
    net$name,"\n",
    net$inf_manager$connection_string,"\n",
    net$tableinfo$dbinfo$nodes,"\n",
    paste(nodes,collapse=" "),"\n",
    paste(envNodes,collapse=" "),"\n",
    number,"\n",
    filename,"\n",
    "1\n",
    paste(evidence,collapse=" "),"\n",
    weightsString,"\n",
    sep="")  
  
  # Run external engine
  system2(
    paste(net$inf_manager$path,"\\EstimateMI.exe",sep=""),
    args=cmdLine,
    wait=FALSE)
  
  # Pause if desired
  Sys.sleep(pause)
}
#----------------
# Principle function (4): Run the batch external MI test on data rows
#----------------
#' Run the batch external MI test
#' 
#' Run the external MI test on a batch of items in a network's data table
#' @param net A network object. The network should have a valid
#' data views.
#' @param first The first item to look at
#' @param last The last item to look at
#' @param nodes The nodes to check mutual information of environment 
#' variables on, in 1-indexed form.
#' @param envNodes The environment variables, in 1-indexed form.
#' @param weights The weights, if a combined MI result desired. Should be one 
#' weight for each node, or NULL. Intended to be probabilities of nodes,
#' though importance weights could be used as well.
#' @param number The number of samples
#' @param filename The full filename to write the results to.
#' @param pause The duration in seconds to wait after making system call
#' @return Nothing. But writes the MI matrix nodes vs envNodes. If a weights 
#' vector has been given, there will be an additional row which
#' is a weighted combination of the other rows.
#' @export
#----------------
performBatchMI<-function(
  net,
  first,
  last,
  nodes,
  envNodes,
  weights,
  number,
  filename,
  pause=2
) {
  # Adjust for zero-index and and -1 instead of NA
  nodes=nodes-1      
  envNodes=envNodes-1
  
  # Set up weights string
  weightsString=""
  if (!is.null(weights)) weightsString=paste(weights,collapse=" ")
  
  # Create command line
  cmdLine<-paste(
    net$name,"\n",
    net$inf_manager$connection_string,"\n",
    net$tableinfo$dbinfo$nodes,"\n",
    paste(nodes,collapse=" "),"\n",
    paste(envNodes,collapse=" "),"\n",
    number,"\n",
    filename,"\n",
    "2\n",
    paste(net$tableinfo$data_views,collapse=" "),"\n",
    first,"\n",
    last,"\n",
    weightsString,"\n",
    sep="")
  
  # Run external engine
  system2(
    paste(net$inf_manager$path,"\\EstimateMI.exe",sep=""),
    args=cmdLine,
    wait=FALSE)
  
  # Pause if desired
  Sys.sleep(pause)
}
#----------------


#----------------
# Help function (helps 1): Make inference settings
#----------------
#' Make external inference settings
#' 
#' Make external inference settings
#' @param net A Bayesian network object
#' @param SystemID A unique ID for the external inference engine session.
#' @param Number The number of samples per inference case
#' @param NextItem The first item to start from. (0-Find Latest)
#' @param Update Whether network parameters should be updated.
#' @param AssumeIntegrity Specifies if engine can assume no other processes will change
#' database records while running.
#' @param StoreImages Whether images should be stored.
#' @param Compression_Setting 'None', 'Until' or 'All'. None means compression value 2 is
#' used through out. All means compression value 1 is used through out. Until will use
#' value 1 until the process catches up with the number of entries in the data table, and
#' value 2 after that.
#' @param Compression_Value1 The number of items to process during an initial catch up
#' @param Compression_Value2 The number of items to process after any initial catch up
#' @param Pause The duration the external inference engine will wait before rechecking
#' if new data has appeared in the data table. 
#' @param DBMS_Timeout Currently unused
#' @return Inference settings object to be passed to makeCommandArguments
#' @keywords internal
#----------------
makeExternalInferenceSettings = function (
  net,
  SystemID,
  Number,             # Number of samples
  NextItem,           # NextItem (0-Find Latest)
  Update,
  AssumeIntegrity,
  StoreImages,
  Compression_Setting,
  Compression_Value1,
  Compression_Value2,
  Pause,
  DBMS_Timeout
) {
  out <- list (
    # System settings
    connection_string=net$inf_manager$connection_string,
    SystemID=SystemID,
    # Database information
    networks=net$tableinfo$dbinfo$networks,
    nodes=net$tableinfo$dbinfo$nodes,
    log=net$tableinfo$dbinfo$log,
    metadata=net$tableinfo$dbinfo$metadata,
    metaresults=net$tableinfo$dbinfo$metaresults,
    # Network information
    network=net$name,
    data_tables=paste(net$tableinfo$data_views,collapse=' '),
    result_tables=paste(net$tableinfo$result_tables,collapse=' '),
    image_table=net$tableinfo$image_table,
    table_length=net$tableinfo$table_length,
    # Core inference settings
    Number=Number,        # Number of samples
    NextItem=NextItem,# NextItem (0-Find Latest)
    Update=Update,
    # Advanced inference settings
    AssumeIntegrity=AssumeIntegrity,
    StoreImages=StoreImages,
    # Compression Settings
    Compression_Setting=Compression_Setting,
    Compression_Value1=Compression_Value1,
    Compression_Value2=Compression_Value2,
    # Timeout Settings
    Pause=Pause,
    DBMS_Timeout=DBMS_Timeout
  )
  class(out)<-'externalInferenceSettings'
  return (out)
}
#----------------
# Help function (helps 1): Make command line arguments
#----------------
#' Make command arguments
#' 
#' Make command arguments. 
#' @param settings Settings list from makeExternalInferenceSettings
#' @return command line string for starting external inference engine
#' @keywords internal
#----------------
makeCommandArguments <- function (
  settings
) {
  return (
    paste(
      # System settings
      settings$connection_string,"\n",
      settings$SystemID,"\n",
      # Database information
      settings$networks,"\n", # Networks table is not needed
      settings$nodes,"\n",
      settings$log,"\n",
      settings$metadata,"\n",
      settings$metaresults,"\n",
      # Network information
      settings$network,"\n",
      settings$data_tables,"\n",
      settings$result_tables,"\n",
      settings$image_table,"\n",
      settings$table_length,"\n",
      # Core inference settings
      settings$Number,"\n",
      settings$NextItem,"\n",
      ifelse(settings$Update,"1","0"),"\n",
      # Advanced inference settings
      ifelse(settings$AssumeIntegrity,"1","0"),"\n",
      ifelse(settings$StoreImages,"1","0"),"\n",
      # Compression Settings
      settings$Compression_Setting,"\n",
      settings$Compression_Value1,"\n",
      settings$Compression_Value2,"\n",
      # Timeout Settings
      settings$Pause,"\n",
      settings$DBMS_Timeout,"\n", 
      sep=""))
}
#----------------








