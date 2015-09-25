


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
#'@export
parent_dir <- function(path = getwd()){
  stringr::str_replace_all(path, "\\/[[:alnum:]]+$", "")
}


#' Generate header 12
#' @export
header12 <- function(vec, sep = " >> "){
  
  if(length(vec) %% 2 != 0){
    stop("length of vector must be even")
  }
  x <- vec[seq(1, length(vec), 2)]
  x <- rep(x, each = 2)
  
  y <- vec[seq(2,length(vec), 2)]
  y <- rep(y, each = 2)
  
  paste(x, y, sep = sep)
}


#' Generate header 23
#' @export
header23 <- function(vec1, vec2, sep = " >> "){
  
  vec_lengths <- sapply(list(vec1, vec2), length)
  
  if(length(unique(vec_lengths)) != 1){
    stop("length of vector must be equal")
  }
  
  if(any(vec_lengths %% 2 != 0)){
    stop("length of vector must be even")
  }
  
  x <- vec1[seq(1, length(vec1), 2)]
  vec1[seq(2, length(vec1), 2)] <- x
  
  paste(vec1, vec2, sep = sep)
}

#' Fills values from left
#' @export
#' @import stringr
left_fill <- function(vec, str, by = NULL) {
  vec <- as.character(vec)
  these <- which(str_detect(vec, str))
  #these <- these[these > 1]
  
  if(is.null(by)){
    vec[these] <- vec[these - 1]
  } else {
    vec[these] <- by
  }
  return(vec)
}


#' Fills values from right
#' @export
#' @import stringr
right_fill <- function(vec, str, by = NULL) {
  vec <- as.character(vec)
  these <- which(str_detect(vec, str))
  #these <- these[these > 1]
  
  if(is.null(by)){
    vec[these] <- vec[these + 1]
  } else {
    vec[these] <- by
  }
  return(vec)
}

#' Build header
#' @export
header <- function(vec1, vec2, sep = " >> ", drop = TRUE){
  
  if(length(vec1) != length(vec2)){
    stop("Vectors must be of equal length", call. = F)
  }
  x <- paste(vec1, vec2, sep = sep)
  
  if(isTRUE(drop)){
    these <- !str_detect(x, perl(sprintf("[[:alnum:]]{1,}%s[[:alnum:]]{1,}", sep)))
    x[these] <- str_replace(x[these], sep, "")
  }
  return(x)
}


#' Rolling fill
#' @export
rolling_fill <-  function(vec, index){
  
  for(i in 1:length(index)){
    vec[index[i]] <- vec[index[i]-1]
  }
  return(vec)
}

#' Creating multiple directories
#' @param vector
#' @export
dirs.create <- function(vec, ...){
  for(i in 1:length(vec)){
    dir.create(vec[i], ...)
     }
}
 