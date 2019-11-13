#' A debug wrapper function for connecting to the SQL Server database.
#' 
#' A debug wrapper function for connecting to the SQL Server database. Should be placed in a separate package.
#' @keywords internal
#' @export
getSQLServerConnectionString <- function () {
  # Originally with non-ASCII characters. Haven't tested without. See commente line below
  # 'DRIVER=SQL Server;UID=Mike;DATABASE=RODBC_TEST;WSID=MIKE-PC;APP=Microsoft® Windows® Operating System;Trusted_Connection=Yes;SERVER=MIKE-PC'
  'DRIVER=SQL Server;UID=Mike;DATABASE=RODBC_TEST;WSID=MIKE-PC;APP=Microsoft Windows Operating System;Trusted_Connection=Yes;SERVER=MIKE-PC'
}
#' A debug wrapper function for connecting to the mySQL database.
#' 
#' A debug wrapper function for connecting to the mySQL database. Should be placed in a separate package.
#' @keywords internal
#' @export
getMySQLConnectionString <- function () {
  "Driver={MySQL ODBC 3.51 Driver};Server=localhost;Port=3306;Database=skfproject;Uid=root;Pwd=root;"
}
#' A debug wrapper function for connecting to the database.
#' 
#' A debug wrapper function for connecting to the database. Should be placed in a separate package.
#' @keywords internal
#' @export
getDebugODBCConnection <- function () {
  cntString<-getMySQLConnectionString()  
  RODBC::odbcDriverConnect(cntString)
}
#' A debug wrapper function for initilizing the database.
#' 
#' A debug wrapper function for initilizing the database. Should be placed in a separate package.
#' @keywords internal
#' @export
debugInitializeDatabase <- function () {
  cn<-getDebugODBCConnection()
  intializeDatabase(cn)
  closeODBCConnection(cn)
}
#' A debug wrapper function for clearing the database.
#' 
#' A debug wrapper function for clearing the database. Should be placed in a separate package.
#' @keywords internal
#' @export
debugClearDatabase <- function () {
  cn<-getDebugODBCConnection()
  clearDatabase(cn)
  closeODBCConnection(cn)
}
#' A debug wrapper function for creating inference settings
#' 
#' A debug wrapper function for creating inference settings. Should be placed in a separate package.
#' @export
#' @keywords internal
makeDebugSettings <- function(
  net,
  SystemID
){
  str<-getMySQLConnectionString()
  #  str<-getSQLServerConnectionString()
  #  path="C:\\Users\\User\\Desktop\\Documents\\Visual Studio 2013\\Projects\\abnInference\\Debug"
  #  path="C:\\Users\\User\\Documents\\Visual Studio 2013\\Projects\\abnInference\\Debug"
  path="C:\\Users\\User\\Desktop\\Documents\\Visual Studio 2013\\Projects\\abnInference\\Release"
  #  path="C:\\Users\\User\\Desktop\\Documents\\ABN_EIE\\"
  externalInferenceSettings(
    path,
    SystemID,
    net,
    str
  )     
}
#' A debug wrapper function for performing MI.
#' 
#' A debug wrapper function for performing MI. Should be placed in a separate package.
#' @export
#' @keywords internal
performMIDebug<-function(
  net,
  evidence,
  nodes,
  envNodes,
  weights,
  number,
  pause
) {
  cnt_string<-getMySQLConnectionString()
  filename<-"C:\\Users\\Maria\\Documents\\mi.txt"
  appPath<-"C:\\Users\\Maria\\Documents\\Visual Studio 2013\\Projects\\abnInference\\Release"
  performMI(
    net,
    cnt_string,
    evidence,
    nodes,
    envNodes,
    weights,
    number,
    filename,
    appPath
  )
  
  Sys.sleep(pause)
  read.table("C:\\Users\\Maria\\Documents\\mi.txt")   
}
#' A debug wrapper function for performing batch MI.
#' 
#' A debug wrapper function for performing batch MI. Should be placed in a separate package.
#' @export
#' @keywords internal
performBatchMIDebug<-function(
  net,
  evidence,
  nodes,
  envNodes,
  weights,
  number,
  pause
) {
  cnt_string<-getMySQLConnectionString()
  filename<-"C:\\Users\\Maria\\Documents\\mi.txt"
  appPath<-"C:\\Users\\Maria\\Documents\\Visual Studio 2013\\Projects\\abnInference\\Release"
  performBatchMI(
    net,
    cnt_string,
    1,
    500,
    nodes,
    envNodes,
    weights,
    number,
    filename,
    appPath
  )
  
  Sys.sleep(pause)
  read.table("C:\\Users\\Maria\\Documents\\mi.txt")   
}
#' A debug wrapper function for stopping external inference.
#' 
#' A debug wrapper function for stopping external inference. Should be placed in a separate package.
#' @export
#' @keywords internal
stopAutomaticInferenceB_debug <- function (
  systemID
) {
  #path="C:\\Users\\Maria\\Documents\\Visual Studio 2013\\Projects\\abnInference\\Debug"
  #path="C:\\Users\\Maria\\Documents\\Visual Studio 2013\\Projects\\abnInference\\Release"
  path="C:\\Users\\User\\Desktop\\Documents\\Visual Studio 2013\\Projects\\abnInference\\Release"
  #  path="C:\\Users\\Maria\\Documents\\ABN_EIE"
  stopAutomaticInferenceB(
    path,
    systemID
  )    
}
