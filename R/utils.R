


# Plotting fun annotations r2
lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}

# char to numeric index


#' Absolute diff
#' @export
absolute_diff <- function(x){
  diff <- abs(x[1] - x[2])
  diff
}



#' Equal length
#' @export
equal_length <- function(x) {
  x.length <- sapply(x, length)
  length(unique(x.length)) == 1
}


#' Parent dir
#'export
parent_dir <- function(path = getwd()){
  stringr::str_replace_all(path, "\\/[[:alnum:]]+$", "")
}

