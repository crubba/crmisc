ger_party_name <- function(vec){

  vec = revalue(vec, c("PDS" = "PDS/DIE LINKE",
                       "FDP" = "F.D.P.",
                       "Die Linke" = "PDS/DIE LINKE",
                       "equal" = "Equally important"))
  
  return(vec)
}
