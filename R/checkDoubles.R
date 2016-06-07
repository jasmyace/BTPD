#' @export checkDoubles
#'   
#' @title Check for primary and secondary balance.

checkDoubles <- function(){
  
  #   ---- Get the information up to this point.  
  assign <- getCellStatus()
  
  doubles <- assign[ !is.na(assign$digiDouble) & assign$digiDouble == 1,]
  
  #   ---- Identify the unique users up to this point.  
  theUsers <- unique(c(doubles$digiPrimary,doubles$digiSecondary))
    
  #   ---- Pretty up the list output from the table.  
  balance <- table(doubles$digiPrimary,doubles$digiSecondary)
  
  return(balance)
}