#' Gets people from input data
#' @inheritParams deforecast

get_people <- function(ppl_loc, ppl_scaling){
  if(ppl_scaling != 1){
     scaled_off <- sample(c(0,1), 
                         length(ppl_loc[,1]), 
                         replace = T, 
                         prob = c(1-ppl_scaling, ppl_scaling)) # removes people from landscape based on probability parameter
    scaled_off[1] <- ifelse(sum(scaled_off) == 0, 1, scaled_off[1]) # makes sure that there is at least one person
    ppl <- cbind(ppl_loc,scaled_off) # binds location of people with whether they're being removed
    ppl <- ppl[ppl[,3] != 0,1:2] # removes people scaled off
  }
  return(ppl)
}
