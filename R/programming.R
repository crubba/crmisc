#' Wrapper around if stop logic
#' @noRd
#' @export
ifstop <- function(cond, mess, ...){
  
  cond <- eval(quote(cond))
  
  if(isTRUE(cond)){
    stop(mess, call. = F)
  }
}


#' Empty object
#' @export
is_empty <- function(x) length(x) == 0

#' To numeric
#' @export
to_numeric <- function(x) UseMethod("to_numeric")

#' @export
to_numeric.character <- function(x) as.numeric(x)

#' @export
to_numeric.factor <- function(x) {
  levels(x) <- 1:length(levels(x))
  x <- as.numeric(as.character(x))
  return(x)
}