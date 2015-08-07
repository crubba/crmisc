#' Take a peek around
#' @export
around <- function(df, row, d = 2){
  df[(row-d):(row+d),]
}


#' Take
#' @export
take <- function(str, re){
  str[str_detect(str, re)]
}

#' List get
#' @export
list_get <- function(l, ext){
  unlist(lapply(l, function(x) x[ext]))
}