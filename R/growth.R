#' Grows forest
#' @inheritParams deforecast
#' @param trees location, age and protected status of trees
#' @param r_l rate of growth adjusted to the ratio of existing trees to maximum capacity of the landscape
#' @param min min age at which trees start to seed
#' @param max max age at which trees finish seeding

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

