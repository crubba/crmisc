#' Print a data frame row and its neighbors
#' @param df the data frame
#' @param row the row index
#' @export
around <- function(df, row, d = 2){
  df[(row-d):(row+d),]
}


#' Extracts elements from a list based on index
#' @param l the list
#' @param ext the index
#' @export
list_get <- function(l, ext, ...){
  unlist(lapply(l, function(x) paste(x[ext], ...)))
}