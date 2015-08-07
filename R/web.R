#' Is str a URL?
#' @noRd
is_url <- function(str){
  grepl("^(http:|https:|www.)", str)
}
