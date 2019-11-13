#-------------------------------
# Primary Function (1): Estimate mutual information 
#-------------------------------
#' Estimate mutual information 
#' 
#' Estimate mutual information on network 
#' given evidence. Note the mutual information of a variable with itself
#' is the entropy of that variable (not necessarily 1).
#' 
#' @param net the network
#' @param evidence vector of integers specifying known values of nodes. This
#' can include NA values for unknown variables.
#' @param nodes The nodes to estimate informative
#' environment variables for.
#' @param envNodes The environment variables.
#' @param weights A weight vector to combine
#' results for weighted overall information.
#' @param num integer specifying the number of samples to used in inference
#' @param algorithm A string specifying a valid sampling algorithm.
#' @param algoSpecific Required settings for sampling algorithm.
#' @param debug For internal use only.
#' @return matrix with each row giving 
#' mutual information of node with environment
#' variables. An additional row exists for the
#' weighted combined information if weights is
#' not NULL.
#' @export
#-------------------------------
getMutualInformation <- function (
  net,
  evidence,
  nodes,
  envNodes,
  weights=rep(1,length(nodes)),
  num=1000,
  algorithm="IS",
  algoSpecific="nat",
  debug=FALSE) {
  # Check evidence vector same length as nodes
  if (length(evidence)!=length(net$nodes)) stop('Evidence/node length mismatch.')
  
  # Perform sampling
  samples<-performSampling(algorithm,net,evidence,num,algoSpecific)
  
  # Create local functions
  extract.likelihood <-function (sample) return(sample$lh)
  extract.sample <-function (sample) return(sample$sample)
  normalize<-function (P) P/sum(P)  
  count<-function (X,W,vals,norm=T) {
    out<-rep(0,vals)
    for (i in 1:length(X)) out[X[i]]<-out[X[i]]+W[i]
    if (norm) return (normalize(out))
    return(out)
  }
  count2<-function (X,Y,W,valsX,valsY,norm=T) {
    out<-matrix(rep(0,valsX*valsY),nrow=valsY)
    for (i in 1:length(X)) out[X[i],Y[i]]<-out[X[i],Y[i]]+W[i]
    if (norm) return (normalize(out))
    return(out)
  }
  
  # Get likelihoods and data
  likelihoods<-sapply(samples,extract.likelihood)
  data<-sapply(samples,extract.sample)
  
  if (debug) {
    print(data)
    print(likelihoods)
    print(cbind(data[nodes,],data[envNodes,],likelihoods))
  }
  
  eProbs<-NULL
  for (i in envNodes) {
    eProbs=rbind(eProbs,
      probs<-count(
        data[i,],
        likelihoods,
        length(net$nodes[[i]]$values),
        T)
    )
  }
#   if (debug) {
#     print("Eprobs")
#     print(eProbs)    
#   }
  
  mutualInformation<-NULL
  for (i in nodes) {
    vec<-c()
    probs<-count(
      data[i,],
      likelihoods,
      length(net$nodes[[i]]$values),
      T)
#     if (debug) {
#       print("Probs")
#       print(probs)      
#     }
    envConnected<-findDConnected(i,envNodes,evidence,net)
    for (j in 1:length(envNodes)) {
      mi<-0
      if (envConnected[j]){
        probs2<-count2(
          data[i,],
          data[envNodes[j],],
          likelihoods,
          length(net$nodes[[i]]$values),
          length(net$nodes[[envNodes[j]]]$values),
          T)
        #       if (debug) {
        #         print("Probs2")
        #         print(probs2)        
        #       }
        #MI=Sum(i,j) p(ij)log(p(ij)/p(i)p(j))
        for (row in 1:nrow(probs2)) {
          for (col in 1:ncol(probs2)) {
            if (probs2[row,col]!=0) {
              mi=mi+probs2[row,col]*
                log2(probs2[row,col]/(probs[row]*eProbs[j,col]))            
            }
          }
        }        
      }
      vec=append(vec,mi)
    }
    mutualInformation<-rbind(mutualInformation,vec)
  }
  if (!is.null(weights)) {
    weights<-normalize(weights)
    combined<-rep(0,length(mutualInformation[1,]))
    for (i in 1:length(nodes)) {
      combined=combined+(mutualInformation[i,]*weights[i])
    }
    mutualInformation<-rbind(mutualInformation,combined)    
  }
  return(mutualInformation)
}
#-------------------------------

#-------------------------------
# Help Function: Get Children of a Node
#-------------------------------
#' Get the children of a node
#'
#' Get the children of a node
#' @param node The node whose children to fetch
#' @param net The network
#' @return A vector of indices of the children of the node
#' @keywords internal
#-------------------------------
getChildren<-function(node,net) {
  out<-c()
  for (n in net$nodes)
    if (node%in%n$parents)
      out<-append(out,n$index)
  return (out)
}
#-------------------------------

#-------------------------------
# Help Function: Get Descendents
#-------------------------------
#' Get Descendents
#' 
#' Get Descendents
#' @param node Index of node
#' @param net The Bayesnet object
#' @return The indices of descendents to the node in the net
#' @keywords internal
getDescendents<-function(node,net) {
  children<-getChildren(node,net)
  out<-children
  for (i in children) out<-union(out,getDescendents(i,net))
  return (out)
}

#' @keywords internal
descendentIn<-function(node,net,set) {
  descendents<-getDescendents(node,net)
}

#' @keywords internal
findDConnected<-function(node,environmental,evidence,net){
#   for (each V ∈ V) {
#     if (V ∈ A)
#       in[V ] = true;
#     else
#       in[V ] = false;
#     if (V is or has a descendent in A)
#       descendent[V ] = true;
#     else
#       descendent[V ] = false;
#   }
  inA_Bool<-!is.na(evidence)
  inA<-which(inA_Bool)
  desA_Bool<-rep(F,length(evidence))
  for (i in 1:length(evidence)) {
    if (i%in%inA || any(inA%in%getDescendents(i,net))) {
      desA_Bool[i]=T
    }
  }
  
# E0 = E ∪ {U → V such that V → U ∈ E};
# ***The above is done in the function call.***
  
# Call Algorithm 2.1 as follows:
# find_reachable_nodes(G0 = (V, E0),B,R);
  reachable<-find_reachable_nodes(net,node,inA_Bool,desA_Bool)

# D = V − (A U R)
  #dSeparated<-setdiff(1:length(evidence),union(inA,reachable))

  environmental%in%reachable
}


#' @keywords internal
getBiEdges<-function(net) {
  out<-NULL
  for (i in net$nodes) {
    if (length(i$parents)>0) {
      out<-rbind(out,cbind(i$parents,i$index,0))
      out<-rbind(out,cbind(i$index,i$parents,0))      
    }
  }
  return(out)
}
#' @keywords internal
getEdgeIndices<-function(node,edges) {
  union(which(edges[,1]==node),which(edges[,2]==node))
}
#' @keywords internal
getLabelledInEdges<-function(node,edges,label) {
  intersect(which(edges[,2]==node),which(edges[,3]==label))
}
#' @keywords internal
getLabelledOutEdges<-function(node,edges,label) {
  intersect(which(edges[,1]==node),which(edges[,3]==label))
}

#' @keywords internal
legal<-function(edgeA,edgeB,net,inA,desA){
  # Use this rule to decide whether (U → V,V → W) is legal in G0:
  # The pair (U → V, V → W) is legal if and only if U != W
  # and one of the following hold:
  # 1) U − V −W is not head-to-head in G and in[V ] is false;
  # 2) U − V −W is head-to-head in G and descendent[V ] is true.
  Apar=edgeA[1]%in%net$nodes[[edgeA[2]]]$parents
  Bpar=edgeB[1]%in%net$nodes[[edgeB[2]]]$parents  
  h2h<-Apar&&!Bpar
  V<-edgeA[2]
  out<-edgeA[1]!=edgeB[2] && (((!h2h)&&!inA[V]) || (h2h&&desA[V]))    
  return (out)
}
#' @keywords internal
find_reachable_nodes <- function (net,node,inA,desA) {
# NB Graph should have bi-directional edges
#   for (each X ∈ B) {
#     add X to R;
#     for (each V such that the edge X → V exists) {
#       add V to R;
#       label X → V with 1;
#     }
#   }
  edges<-getBiEdges(net)
  nEdges<-getEdgeIndices(node,edges)
  out<-union(edges[nEdges,1],edges[nEdges,2])
  edges[nEdges,3]=1

#   i = 1;
#   found = true;
# while (found) {
#   found = false;
#   for (each V such that U → V is labeled i)
#     for (each unlabeled edge V → W
#          such that (U → V ,V → W) is legal) {
#       add W to R;
#       label V → W with i +1;
#       found = true;
#     }
#   i = i +1;
# }
  i = 1
  found = T
  while (found) {
    found = F
    uvEdges<-which(edges[,3]==i)
    for (uv in uvEdges) {
      vwEdges<-getLabelledOutEdges(edges[uv,2],edges,0)
      for (row in vwEdges) {
        if (legal(edges[uv,1:2],edges[row,1:2],net,inA,desA)) {
          out<-union(out,edges[row,2])
          edges[row,3]=i+1
          found=T
        }
      }
    }
    i = i +1;
  }

  return(out)
}