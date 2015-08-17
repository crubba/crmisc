#' Run the mode
#' @export
run_model <- function(path, party = NULL, party_save_str = NULL, chains = 1,
                      iter, seed = sample(1:100, length(chains))){

  # -----------
  # Force arguments
  # -----------
  force(path)
  force(party)
  force(party_save_str)
  force(chains)
  force(iter)
  force(seed)
  
  path <- normalizePath(path)
  
  if(is.null(party)) stop("specify party")
  if(is.null(iter)) stop("specify iter")
  
  source(path, local = T)
}

#' plot the mode
#' @export
plot_model <- function(path, data_dir){
  source(path, local = T)
}


#' Create table of convergence diagnostics
#' @export
stantab <- function(obj){
  
}


#' Converts stan to coda object
#' @export
#' @import coda
#' @details Adopted from Jeromy Anglim (http://jeromyanglim.tumblr.com/post/91434443911/how-to-convert-a-stan-fit-object-to-work-with-coda)
stan2coda <- function(fit) {
  mcmc.list(lapply(1:ncol(fit), function(x) mcmc(as.array(fit)[,x,])))
}