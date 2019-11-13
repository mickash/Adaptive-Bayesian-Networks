# d-deep, s-shallow, u-unique, m-multiple networks sharing
#-------
# 
#-------
# 1 2 3 4 5 6 7 8 9s 9d 10s 10d 11s 11d 12s 12d 13u 13m 14u 14m 15 16 17 18 19 20 
# 21 22 23 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
#

#' Complete Package Test
#' 
#' Test all functionality in package.
#' Work on this function is ongoing.
#' @export
CompletePackageTest=function(
  clear=T,
  connection_string=getMySQLConnectionString(),
  external_path="C:\\Users\\User\\Documents\\Visual Studio 2013\\Projects\\abnInference\\Release",
  seed=NULL
  ) {
  if (!is.null(seed)) set.seed(seed)
  
  # 1 Close All ODBC Connections
  closeAllODBCConnections()
  
  # 2    Get connection 42                                               # YES
  cn=getODBCConnection(connection_string)       
  # 3  Create BD Info 3                                                  # YES  
  dbinfo=dbmsinfo() 
  # 4  Clear Database 1                                                  # YES
  if (clear) clearDatabase(cn,dbinfo)                             
  # 5  Initialize Database and retrieve DBinfo object 2 R3               # YES  
  dbinfo=initializeDatabase(cn)
  # 6  Create Bayesnet and check inference manager 5 8                   # YES
  net=bayesnet("net1",dbinfo,connection_string,external_path)
  
  # 7  (Shallow) Add nodes to Bayesnet 9s, 17 (16,18,19,20,21,22)        # YES
  net = addNode(net, 'noisyor', c('Off','On'), 'var1', c(), matrix( c(1e6,1e3), nrow=1))  
  net = addNode(net, 'noisyor', c('Off','On'), 'var2', c(), matrix( c(1e6,1e3), nrow=1)) 
  net = addNode(net, 'noisyor', c('Off','On'), 'var3', c(), matrix( c(1e6,1e3), nrow=1))   
  net = addNode(net, 'noisyor', c('Off','On'), 'var4', c(), matrix( c(1e6,1e3), nrow=1)) 
  net = addNode(net, 'noisyor', c('Off','On'), 'var5', c(), matrix( c(1e6,1e3), nrow=1)) 
  net = addNode(net, 'noisyor', c('Off','On'), 'var6', c(), matrix( c(1e6,1e3), nrow=1)) 
  net = addNode(net, 'logand', c('Off','On'), 'la1', c(1,3), matrix( c(2,2), nrow=1)) 
  
  # 8  (Shallow) Add nodes and edges to Bayesnet 11s                     # YES
  net = addEdge(net,2,1,matrix(c(1,10),nrow=1 )) 
  net = addEdge(net,4,2,matrix(c(1,10),nrow=1 )) 
  net = addEdge(net,4,7,matrix(c(1,10),nrow=1 )) 
  net = addEdge(net,5,7,matrix(c(1,10),nrow=1 )) 
  net = addEdge(net,6,1,matrix(c(1,10),nrow=1 )) 
  
  # 9  (Shallow) Remove some edges from Bayesnet 12s                     # YES 
  net = removeEdge(net,6,1) 

  # 10  (Shallow) Remove some nodes and edges from Bayesnet 10s           # YES
  net = removeNode(net,6) 
  
  # 11  Create analysis object 38                                        # YES
  analyzer=analyzer(matrix(c(2,3,2,2),ncol=2),matrix(c(4,5,2,2),ncol=2))

  # 12  Plot network 41                                                  # YES
  plot(net)

  # 13  Get summary of network 23                                        # YES
  summary(net)
  
  # 14  Serialize Network and examine tableinfo 27 7                    # YES
  net=serializeNetwork(cn,net)                             

  # 15  Write data 14u                                                  # YES
  for (i in 1:100) {
    s=sample(c(NA,1,2),length(net$nodes),replace=T)
    s[6]=NA # Since it is a logical node
    writeData(cn,s,net)
  }
  
  # 16  Read written Data 13u
  for (i in 1:10) print(readData(cn,net,i))
  
  # 17  PerformRInference and examine posterior object 36 44                 # YES*
  # Testing update=T
  # Currently only tested IS - nat
  net=predict(net,readData(cn,net,1),1000,T,"IS",algoSpecific="nat") # should go to predict.bayesnet
  print(net$posterior)
  for (i in 1:5) print(net$nodes[[i]]$params)
  
  # 18  Analyze Posterior Probabilities 39 40                                #YES
  analyze(analyzer,net,net$posterior)
  net$posterior=NULL
  
  # 19  Perform R MI 37                                                      # NO
  print(getMutualInformation(net,rep(NA,6),c(4,5),c(1,2)))
  
  # 20  Start External Inference 32
  # Test compression too                                                        #YES
  net=startExternalInference(net,Compression_Setting = 'Until',pause=20)

  # 21  Stop External Inference 33                                           #YES
  net=stopAutomaticInference(net)    
  
  # 22  Read Result and examine posteriori object 13 R44                     #YES
  posterior=readResults(cn,net,1)
  analyze(analyzer,net,posterior)
  
  # 23  Record item/time for RR and Store Image 31                           #YES
  time1=Sys.time()
  storeImage(cn,net)
  
  # 24  Rewind Readapt to original image 30                                  #YES
  for (i in 1:100) {
    s=sample(c(NA,1,2),length(net$nodes),replace=T)
    s[6]=NA # Since it is a logical node
    writeData(cn,s,net)
  }
  net=startExternalInference(net,pause=20)
  net=stopAutomaticInference(net)  
  time2=Sys.time()
  # Primary Test:
  net=rewindReadapt(cn,net,time1,time2,c(1,2),c(1,0))
  net=startExternalInference(net,pause=20)
  net=stopAutomaticInference(net)
  
  # 25  Restore previous image 29                                            #YES
  time3=Sys.time()
  storeImage(cn,net)
  for (i in 1:100)  {
    s=sample(c(NA,1,2),length(net$nodes),replace=T)
    s[6]=NA # Since it is a logical node
    writeData(cn,s,net)
  }
  net=startExternalInference(net,pause=20)                                      
  net=stopAutomaticInference(net)
  storeImage(cn,net)
  # Primary Test:
  net=restoreImage(cn,net,time3)
  
  # 26  Clone network: net2 4                                                #YES
  net2=cloneBayesnet(net,"net2")
  
  # 27  Unserialize net2 28                                                  #YES
  net2=serializeNetwork(cn,net2)
  net2=unserializeNetwork(cn,net2,F,F)
  
  # 28  Remove nodes from net2 10d                                           #YES
  net2=serializeNetwork(cn,net2)
  net2=removeNode(net2,5,cn,T)
  
  # 29  Add nodes to net2 9d                                                 #YES
  net2 = addNode(net2, 'noisyor', c('Off','On'), 'var7', c(), matrix( c(1e6,1e3), nrow=1),
                 cn=cn,deepUpdate=T) 
  
  # 30  Add edges to net2 11d                                                #YES
  net2 = addEdge(net2,4,3,matrix(c(1,10),nrow=1 ),cn=cn,deepUpdate=T) 
  
  # 31  Remove edges from net2 12d                                           #YES
  net2 = removeEdge(net2,2,1,cn=cn,deepUpdate=T) 
  
  # 32  Write data through net2 14m                                          #YES
  for (i in 1:100)  {
    s=sample(c(NA,1,2),length(net$nodes),replace=T)
    s[6]=NA # Since it is a logical node
    writeData(cn,s,net)
  }
  
  # 33  Read data through net2 13m                                           #YES
  for (i in 90:100) print(readData(cn,net2,i))
  
  # 34  GetNetwork1 6                                                        #YES
  net3=getNetwork(cn,net$name,connection_string,external_path)
  
  # 35  Start MI 34
  performMI(net3,rep(NA,length(net$nodes)),c(4,5),c(1,2),c(1,1),10000
        ,"c:/users/user/documents/testmi1.txt",20)
  print(read.table("c:/users/user/documents/testmi1.txt"))  
  
  # 36  Start Batch MI 35 
  performBatchMI(net3,101,110,c(4,5),c(1,2),c(1,1),10000
    ,"c:/users/user/documents/testmi2.txt",20)
  print(read.table("c:/users/user/documents/testmi2.txt"))  

  # 37 Update layout
  net2$layout=matrix(rep(1:length(net2$nodes),2),ncol=2)
  updateNetworkLayout(cn,net2)
  
  # 38 Restore and Overwrite
  net2_=restoreAndOverwrite(cn,"net1","net2",connection_string,external_path)
  
  # 39 Plot updated layout
  plot(net2_)
  
  # No primary                                         
  for (i in 1:100)  {
    s=sample(c(NA,1,2),length(net$nodes),replace=T)
    s[6]=NA # Since it is a logical node
    writeData(cn,s,net)
  }
  net2_=startExternalInference(net2_,pause=20)
  net2_=stopAutomaticInference(net2_)
  
  # 40  Get Networks 45
  print(getNetworks(cn))
  
  # 41  Get Data Families 46
  print(getDataFamilies(cn))
  
  # 42  Get Log Entries 47
  print(getLogEntries(cn))
  
  # 43  Clear Image Table  25
  clearAllImages(cn,net)
  
  # 44  Clear Results Tables 24
  clearAllResults(cn,net)
  
  # 45  Clear Data Tables 26
  clearAllData(cn,net,T)
  
  # 46  Close Connection 43
  closeODBCConnection(cn)
}