#' Grows forest
#' @param r_l Logistic rate of growth
#' @param trees Age and location profile of trees
#' @param min Minimum age at which trees can seed
#' @param max Max age at which trees stop seeding
#' @export

growth <- function(trees, 
                   r_l,
                   min,
                   max){
  
    age <- trees[,3];
    age[age < min] = 0;
    age[age > max] = 0;
    age[age != 0] <- 1;
    new <- subset(cbind(trees, age), age != 0);
    n_new <- ceiling(length(trees[,3]) * r_l);
    new <- randomRows(trees, n_new,age); # gets random rows from the matrix of trees which can seed
    new[,4] <- 0; # age of new trees is 0
    newtrees <- rbind(trees, new[,1:4]); # combines old and new trees
    newtrees <- matrix(
        newtrees[order(newtrees[,4],decreasing = TRUE), ], 
        ncol = 4
      )
  return(newtrees)
}

