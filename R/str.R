#' Remove white space
#' @export
rm_str_white <- function(el) gsub("^\\s+|\\s+$", "", el)