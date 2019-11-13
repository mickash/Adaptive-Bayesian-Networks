#' Calculate the row in parameters for given parent values
#' 
#' Calculate the row in parameters for given parent values
#' @param cur vector specifying values of parents
#' @param max vector specifying number of values of parents
#' @return integer specifying row index
#' @keywords internal
calculate.row <- function (cur,max) {
  row <- 1
  l<-length(cur)
  if (l>0) {
    jump<-1
    for (i in length(cur):1) {
      row=row+(cur[i]-1)*jump
      jump=jump*max[i]
    }
  }
  return (row)
}
#' Generate value from parameters row
#' 
#' Generate value from parameters row
#' @param params row vector from parameters matrix to be used
#' @return integer specifying generated value
#' @keywords internal
genFromRow <- function (params) {
  s<-sum(params)
  v<-runif(1,0,s)
  for (i in 1:length(params)) {
    if (v<=params[i]) {
      return (i)
    } else {
      v=v-params[i]
    }
  }
  stop('Failed to generate value!')
}
#' Calculate dirichlet probability
#' 
#' Calculates the proportion of a particular entry in a vector to the sum of all
#' entries. Use in calculating MAP values of probability parameters in multinomial
#' dirstributions with dirichlet hyperparameters.
#' @param i index of value
#' @param v vector of values
#' @return v[i]/sum(v)
#' @keywords internal
calculate.dprob <- function (i,v) {
  v[i]/sum(v)
}







