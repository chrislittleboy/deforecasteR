#' Does the felling (peoples' budgets change and trees cease to exist)
#' @inheritParams deforecast
#' @param trees location, age and protected status of trees
#' @export

rate <- function(trees, k, r) {
k <- k * max(trees[,1]) * max(trees[,2]);
n <- length(trees[,1]);
s <- n + (r * n) * (1 - n/k);
r_l <- (s-n)/n;
return(r_l)}