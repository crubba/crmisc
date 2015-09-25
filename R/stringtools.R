#' Takes elements from a character vector based on Regular Expression
#' @export
#' @import stringr
#' @examples
#' vec <- c("01", "02", "3", "4", "05")
#' take(vec, "^0")
take <- function(str, re){
  str[str_detect(str, re)]
}


#' Converts a string to lowercase based on index
#' @export
#' @examples 
#' lowercase("Dr. Dre", 1)
#' lowercase("Dr. Dre")
lowercase <- function(str, index = NULL){
  
  if(is.null(index)){
    index <- 1:nchar(str)
  }
  
  x <- strsplit(str, "")
  
  x <- lapply(x, function(xx){
    if(length(xx) >= index){
    xx[index] <- sapply(xx[index], tolower)
    } else {
      warning("index > length", call. = F)
      }
    xx <- paste(xx, collapse = "") 
  })
  return(unlist(x))
}

#' Converts a string to uppercase based on index
#' @export
#' @examples 
#' uppercase("Dr. Dre", 2)
#' uppercase("Dr. Dre", -1)
uppercase <- function(str, index = NULL){
  
  if(is.null(index)){
    index <- 1:nchar(str)
  }
  
  x <- strsplit(str, "")
  
  x <- lapply(x, function(xx){
    if(length(xx) >= index){
      xx[index] <- sapply(xx[index], toupper)
    } else {
      warning("index > length", call. = F)
    }
    xx <- paste(xx, collapse = "") 
  })
  return(unlist(x))
}

#' Remove white space from string
#' @export
rm_str_white <- function(el) gsub("^\\s+|\\s+$", "", el)