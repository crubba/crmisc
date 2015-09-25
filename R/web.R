#' Is string a URL?
#' @param str the string
#' @export
is_url <- function(str){
  grepl("^(http:|https:|www.)", str)
}