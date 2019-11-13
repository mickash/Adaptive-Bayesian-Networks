validate<-function(object) {
  if (length(object$values)<2) {
    stop('Invalid values.')
  }
  if (any(duplicated(object$parents))) {
    stop('Duplicated parent indices.')
  }
  
  max.p <- my.max(object$parents)
  min.p <- my.min(object$parents)
  if (object$index<=max.p || object$index<1) {
    stop('Invalid index')
  }
  if (length(object$parents)>0 && min.p < 1) {
    stop('Invalid parent index')    
  }
  if (length(object$pvals)>0 && min(object$pvals)<2) {
    stop('Invalid number of parent values.')
  }
  if (length(object$parents)!=length(object$pvals)) {
    stop('Parent indices and values vectors are of different lengths.')
  }
  UseMethod('validate', object)
}

#' Validate parameters of dirichlet node
#' 
#' Validate parameters of dirichlet node
#' @keywords internal
validate.dirichlet<-function(object) {
  if (length(object$params)!=prod(object$pvals)*length(object$values)) {
    stop('Invalid parameters.')
  }
}

#' Validate inputs for logical and node constructor
#' 
#' Validate inputs for logical and node constructor
#' @keywords internal
validate.logand <- function (object) {
  if (length(object$params)!=length(object$pvals) || length(object$parents)!=2) {
    stop('Logical-And nodes require two parents, a specification of the number of values these parents have, and a specification of the values relevant for the logical and function.')
  }
  if (!is.null(object$params2)) {
    stop('Logical-And nodes do not have a second parameter matrix.')    
  }
}

#' Validate parameters of flag node
#' 
#' Validate parameters of flag node
#' @keywords internal
validate.flag <- function (object) {
  if (length(object$parents)!=1 || length(object$params)!=1) {
    stop('Flag nodes require one parents, a specification of the number of values this parent has as a single valued vector, and a specification of the value relevant for the flag function as a 1x1 matrix as params.')
  }
  if (!is.null(object$params2)) {
    stop('Flag nodes do not have a second parameter matrix.')    
  }
}

#' Validate parameters of noisyor node
#' 
#' Validate parameters of noisyor node
#' @keywords internal
validate.noisyor <- function (object) {
  if (length(object$pvals)==0) {
    if (length(object$params)!=length(object$values)+1) {
      stop('Parent values and parameters mismatch.')
    }
  } else {
    if (((sum(object$pvals-1)+1)*(length(object$values)+1))!=length(object$params)) {
      stop('Parent values and parameters mismatch.')
    }
  }
  if (length(object$values)==2) {
    if (!is.null(object$params2)) {
      stop('Binary noisy-or nodes do not have a second parameter matrix.')          
    }
  } else {
    if (
      (ncol(object$params2)!=length(object$values)) ||
        (nrow(object$params2)!=length(object$values)^(1+length(object$parents)))    
    ) {
      stop('Invalid second parameter matrix.')    
    }
    
  }  
}

#' Validate parameters of logprod node
#' 
#' Validate parameters of logprod node
#' @keywords internal
validate.logprod <- function (object) {
  if (length(object$parents)!=2) {
    stop('Logprod nodes require two parents.')
  }
  if (length(object$values)!=prod(object$pvals)) {
    stop('Logprod nodes require values for each combination of parent values')
  }
  if (!is.null(object$params)) {
    stop('Logprod nodes do not have a parameter matrix.')    
  }
  if (!is.null(object$params2)) {
    stop('Logprod nodes do not have a second parameter matrix.')    
  }
}
#' Validate parameters of flagprod node
#' 
#' Validate parameters of flagprod node
#' @keywords internal
validate.flagprod <- function (object) {
  if (length(object$parents)!=2) {
    stop('Flagprod nodes require two parents.')
  }
  if (length(object$values)!=4) {
    stop('Flagprod nodes require four values.')
  }
  if (length(object$params)!=2) {
    stop('Flagprod require specification of parent values involved.')    
  }
  if (!is.null(object$params2)) {
    stop('Flagprod nodes do not have a second parameter matrix.')    
  }
}
#' Validate parameters of logor node
#' 
#' Validate parameters of logor node
#' @keywords internal
validate.logor <- function (object) {
  if (length(object$parents)!=2) {
    stop('Logor nodes require two parents.')
  }
  if (length(object$values)!=2) {
    stop('Logor nodes require two values.')
  }
  if (length(object$params)!=2) {
    stop('Logor nodes require specification of parent values involved.')    
  }
  if (!is.null(object$params2)) {
    stop('Logor nodes do not have a second parameter matrix.')    
  }
}
