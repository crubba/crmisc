#' Encode german
#' @export
encode_german <- function(x, type = NULL){
  
  if(is.null(type)) stop("please specify type argument")
  
  if(type == "umlaut"){
    x <- str_replace_all(x, "oe", "ö")
    x <- str_replace_all(x, "ue", "ü")
    x <- str_replace_all(x, "ae", "ä")
    x <- str_replace_all(x, "ß", "ss")
  }
  
  if(type == "noumlaut"){
    x <- str_replace_all(x, "ö", "oe")
    x <- str_replace_all(x, "ü", "ue")
    x <- str_replace_all(x, "ä", "ae")
    x <- str_replace_all(x, "ß", "ss")
    x <- str_replace_all(x, "é", "e")
  }
  
  return(x)
}

spec_char <- function(str){
  
  str <- str_replace_all(str, " \\\x96 ", "-")
  str <- str_replace_all(str, "\\\U3e36663c", "ö")
  str <- str_replace_all(str, "\\\U3e63663c", "ü")
  str <- str_replace_all(str, "\\\xe4", "ä")
  str <- str_replace_all(str, "\\\xdf", "ß")
  
  str
}
