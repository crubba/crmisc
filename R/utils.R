list_get <- function(l, ext){
  lapply(l, function(x) x[ext]) %>% unlist
}


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

to_numeric <- function(x){
  x <- as.factor(x)
  levels(x) <- 1:length(levels(x))
  x <- as.numeric(as.character(x))
}


# Take a peek around
around <- function(df, row, d = 2){
  df[(row-d):(row+d),]
}
  

# Take
take <- function(str, re){
  str[str_detect(str, re)]
}

# Absolute diff
absolute_diff <- function(x){
  diff <- abs(x[1] - x[2])
  diff
}
