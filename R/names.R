#' Extract name
#' @export 
extract_name <- function(x, which = NULL){
  
  if(is.null(which)) stop("must specify name argument")
    
    if(which == "first"){
      name <- str_split(x, ", ") %>% lapply(., function(pp) return(pp[2])) %>% unlist
      name <- str_replace(name, "dr. ", "")
      name <- str_replace(name, "prof. ", "")
      name <- str_trim(name)
    }
  
  if(which == "last"){
    name <- str_split(x, ", ") %>% lapply(., function(pp) return(pp[1])) %>% unlist
    name <- str_trim(name)
  }
  
  return(name)
}


clean_name <- function(name){
  name <- str_replace_all(name, "\\n", " ")
  name <- str_replace_all(name, " \\(.+\\)", "")
  return(name)
}

