#' Makes trees
#' @importFrom stats quantile rnorm aggregate
#' @inheritParams deforecast
#' @return A 2-member list with protected and unprotected trees.

get_trees <- function(p_loc, # 2 column matrix (x/y)
                      np_loc, # 2 column matrix (x/y)
                      mean_age, # integer 
                      sd_age) # integer
  {
  
# gets age of trees (normally distributed)
age_p <- rnorm(n = length(p_loc[,1]), mean = mean_age, sd = sd_age) 
age_p[age_p < 0] <- 0 # negative ages to 0
age_p <- round(age_p, digits = 0) # rounds to nearest year
p_loc <- matrix(cbind(p_loc, age_p, rep(1, length(age_p))), ncol = 4)

age_np <- rnorm(n = length(np_loc[,1]), mean = mean_age, sd = sd_age)
age_np[age_np < 0] <- 0
age_np <- round(age_np, digits = 0)
np_loc <- matrix(cbind(np_loc, age_np, rep(0, length(age_np))), ncol = 4)

trees <- rbind(p_loc,np_loc);
trees <- data.frame(matrix(unlist(matrix(trees)), ncol = 4))
return(trees);
}
