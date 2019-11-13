#-------------------------------------
# Primary Function (1): Initialize Database
#-------------------------------------
#' Create required tables for a database.
#' 
#' Create required tables for a database.
#' @param cn An open RODBC connection.
#' @param networks The desired name of the database's network table.
#' @param nodes The desired name of the database's nodes table.
#' @param log The desired name of the database's log table.
#' @param metadata The name of the database's metadata table.
#' @param metaresults The name of the database's metaresults table.
#' @return A database information object
#' @export
#-------------------------------------
initializeDatabase <- function (
  cn,
  networks="networks",
  nodes="nodes",
  log="log",
  metadata="metadata",
  metaresults="metaresults"
  ) {
  createNetworksTable(cn,networks)
  createNodesTable(cn,nodes,networks)
  createLogTable(cn,log)
  createMetadataTable(cn,metadata) 
  createMetaresultsTable(cn,metaresults) 
  dbmsinfo(networks,metadata,nodes,log,metaresults)
}
#-------------------------------------
# Primary Function (2): Clear Database
#-------------------------------------
#' Clear all ABN tables in a database.
#' 
#' Clear all ABN tables in a database.
#' @param cn An open RODBC connection.
#' @param dbInfo A database information object returned from intializeDatabase
#' @export
#-------------------------------------
clearDatabase <- function (
  cn,
  dbinfo
) {  
  # Drop Network Tables except data
  df=SqlQuery(cn,paste("SELECT * FROM ",dbinfo$networks,sep=""))
  if (nrow(df)>0)
    for (i in 1:nrow(df)) {
      # Drop Network Table
      SqlQuery(cn,paste("DROP TABLE IF EXISTS ",df[i,1],sep=""))   
      # Drop Data Views
      data_views=parseWSSS_asStringVector(df[i,3])
      for (view in data_views) 
        SqlQuery(cn,paste("DROP VIEW IF EXISTS ",view,sep=""))   
      # Drop Result Tables
      result_tables=parseWSSS_asStringVector(df[i,4])
      for (table in result_tables) 
        SqlQuery(cn,paste("DROP TABLE IF EXISTS ",table,sep=""))   
      # Drop Image Table
      SqlQuery(cn,paste("DROP TABLE IF EXISTS ",df[i,5],sep=""))   
    }
  # Drop Network Data Tables
  df=SqlQuery(cn,paste("SELECT * FROM ",dbinfo$metadata,sep=""))
  if (nrow(df)>0)
    for (i in 1:nrow(df))     
      SqlQuery(cn,paste("DROP TABLE IF EXISTS",df[i,1]))   
  # Drop Node Table
  SqlQuery(cn,paste("DROP TABLE IF EXISTS ",dbinfo$nodes,sep=""))
  # Drop Log Table
  SqlQuery(cn,paste("DROP TABLE IF EXISTS ",dbinfo$log,sep=""))
  # Drop Network Table
  SqlQuery(cn,paste("DROP TABLE IF EXISTS ",dbinfo$networks,sep=""))
  # Drop Metadata Table
  SqlQuery(cn,paste("DROP TABLE IF EXISTS ",dbinfo$metadata,sep=""))
  # Drop Metaresults Table
  SqlQuery(cn,paste("DROP TABLE IF EXISTS ",dbinfo$metaresults,sep=""))
}
#-------------------------------------

#-------------------------------------
# Help Function (1.1): Create network table
#-------------------------------------
#' Create a database's network table
#' 
#' Create a database's network table
#' @param cn An open RODBC connection
#' @param table The string to use as the name of the 
#' network table. This should be unique in the database.
#' @keywords internal
#-------------------------------------
createNetworksTable <- function (
  cn,
  table
  ) {
  str<-paste("CREATE TABLE ",table," (
             Network VARCHAR(50) NOT NULL PRIMARY KEY,
             DataFamily VARCHAR(500) NOT NULL,
             DataViews VARCHAR(500) NOT NULL,
             ResultTables VARCHAR(500) NOT NULL,
             Images VARCHAR(50) NOT NULL,
             Datamap VARCHAR (1000),
             Layout VARCHAR(1500),
             TableLength INT)"
             ,sep="")
  SqlQuery(cn,str)  
}
#-------------------------------------
# Help Function (1.2): Create log table
#-------------------------------------
#' Create a database's log table
#' 
#' Create a database's log table
#' @param cn An open RODBC connection.
#' @param table The string to use as the name of the log table.
#' This should be unique in the database.
#' @keywords internal
#-------------------------------------
createLogTable <- function (
  cn,
  table
  ) {
  str<-paste("CREATE TABLE ",table," (
             Item int NOT NULL AUTO_INCREMENT PRIMARY KEY,
             Time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
             Network VARCHAR(50) NOT NULL,
             Code int NOT NULL,
             Message VARCHAR(15000))"
             ,sep="")
  SqlQuery(cn,str)
}
#-------------------------------------
# Help Function (1.3): Create nodes table
#-------------------------------------
#' Create a database's nodes table
#' 
#' Create a database's nodes table
#' @param cn An open RODBC connection.
#' @param table The string to use as the name of the nodes
#' table. This should be unique in the database.
#' @param networks The name of the network table in
#' the database.
#' @keywords internal
#-------------------------------------
createNodesTable <- function (cn,table,networks) {
  str<-paste("CREATE TABLE ",table," (
             Network VARCHAR(50) NOT NULL,
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
             PRIMARY KEY(Network,NodeIndex),
             FOREIGN KEY(Network) REFERENCES ",networks,"(Network))",
             sep="")
  SqlQuery(cn,str)  
}
#-------------------------------------
# Help Function (1.4): Create metadata table
#-------------------------------------
#' Create a database's metadata table
#' 
#' Create a database's metadata table
#' @param cn An open RODBC connection.
#' @param table The string to use as the name of the metadata
#' table. This should be unique in the database.
#' @keywords internal
#-------------------------------------
createMetadataTable <- function (cn,table) {
  str<-paste("CREATE TABLE ",table," (
             DataTable VARCHAR(50) NOT NULL PRIMARY KEY,
             Family VARCHAR(50) NOT NULL,
             NodeNames VARCHAR(5000) NOT NULL,
             NodeValues VARCHAR(5000) NOT NULL,
             TableLength INT NOT NULL)",
             sep="")
  SqlQuery(cn,str)  
}
#-------------------------------------
# Help Function (1.5): Create metaresults table
#-------------------------------------
#' Create a database's metaresults table
#' 
#' Create a database's metaresults table
#' @param cn An open RODBC connection.
#' @param table The string to use as the name of the result data
#' table. This should be unique in the database.
#' @keywords internal
#-------------------------------------
createMetaresultsTable <- function (cn,table) {
  str<-paste("CREATE TABLE ",table," (
             ResultTable VARCHAR(50) NOT NULL PRIMARY KEY,
             NodeNames VARCHAR(5000) NOT NULL,
             NodeValues VARCHAR(5000) NOT NULL,
             TableLength INT NOT NULL)",
             sep="")
  SqlQuery(cn,str)  
}
#-------------------------------------

