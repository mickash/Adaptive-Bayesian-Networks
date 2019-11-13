calculate.torder <- function (net) {
  bucket <- 1:length(net$nodes)
  out<-c()
  while (length(bucket)>0) {
    old.bucket<-bucket
    for (i in 1:length(net$nodes)) {
      if (i %in% bucket & all(!net$nodes[[i]]$parents %in% bucket)) {
        out<-append(out,i)
        bucket<-bucket[-which(bucket==i)]
      }
    }
    if (identical(bucket,old.bucket)) {
      stop("Could not perform topological ordering.")
    }
  }
  return (out)
}