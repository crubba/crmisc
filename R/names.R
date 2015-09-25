#' Extracts parts from character vectors of names using simple heuristics
#' @param x a character vector
#' @param which the part of the name to be extracted. "last" extracts family names, "first" first names.
#' @export 
#' @examples
#' extract_name("Dagobert Duck", "last")
#' extract_name("Dr. Martin Luther King", "first")
extract_name <- function(x, which = NULL){
  
  if(is.null(which)) stop("must specify name argument")
	
	if(which == "first"){
		#name <- str_split(x, ", ") %>% lapply(., function(pp) return(pp[2])) %>% unlist
    name <- str_replace(x, "[dD]{1}r. ", "")
    name <- str_replace(name, "[Pp]{1}rof[\\.]? ", "")
    name <- str_trim(name)
    name <- str_replace(name, " [[:alpha:]-]+$", "")
    }
  
  if(which == "last"){
  	name <- str_replace(x, "dr. ", "")
  	name <- str_replace(name, "prof. ", "")
  	name <- str_trim(name)
  	name <- str_extract(name, "[[:alpha:]-]+$")
  }
  
  return(name)
}

#' Cleans name
#' @export
clean_name <- function(name){
  name <- str_replace_all(name, "\\n", " ")
  name <- str_replace_all(name, " \\(.+\\)", "")
  return(name)
}

