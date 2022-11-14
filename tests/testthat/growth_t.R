# creates random plausible set of parameters

xdim = sample(c(50:2000), 1)
ydim = sample(c(50:2000))
explicit = sample(c(TRUE,FALSE), 1) 
ppl_n = sample(c(50:2000), 1)
ppl_loc = sample(list(cbind(sample(xdim,ppl_n, replace = T),sample(ydim,ppl_n, replace = T)), NULL), 1)[[1]]
ppl_scaling = sample(c(1:1000)/1000, 1)
budget = sample(c(200:5000), 1)
p_n = sample(c(1:1000), 1)
np_n = sample(c(1:1000), 1)
p_loc = sample(list(cbind(sample(xdim,p_n, replace = T),sample(ydim,p_n, replace = T)), NULL), 1)[[]]
np_loc = sample(list(cbind(sample(xdim,np_n, replace = T),sample(ydim,np_n, replace = T)), NULL),1)[[]]
mean_age = sample(c(10:150), 1)
sd_age = sample(c(1:50), 1)
travel_cost = sample(c(0:100), 1)
management_cost = sample(c(0:1000), 1)
value = sample(c(100:10000), 1)
mobility = sample(c(1:1000)/1)
k = sample(c(50:100)/100, 1)
ppl_scaling = sample(c(1:500)/1000, 1)
maturity = sample(c(0:round(mean_age/1.5, digits = 0)), 1) 
max_age = sample(c(round(mean_age * 1.5, digits = 0):mean_age + sd_age * 3),1)
years = sample(c(0:10), 1)

if(explicit == FALSE) {
  people <- make_people(
    xdim = xdim,
    ydim = ydim,
    ppl_n = ppl_n); # makes the people
} else { 
  people <- get_people(
    ppl_loc = ppl_loc, # or gets the people
    ppl_scaling = ppl_scaling)
}

if(explicit == FALSE){
  trees <- make_trees(
    xdim = xdim,
    ydim = ydim,
    p_n = p_n,
    np_n = np_n,
    mean_age = mean_age,
    sd_age = sd_age); # makes the trees
} else {
  trees <- get_trees(
    p_loc = p_loc,
    np_loc = np_loc,
    mean_age = mean_age,
    sd_age = sd_age # or gets the trees
  )  
}

# creates two null list to store the results of the simulation for each year    

ppl_results <- vector(mode = "list", length = years + 1);
tree_results <- vector(mode = "list", length = years + 1);

# stores the initial conditions of the people and trees 

# controlling for if nobody is on the landscape

ppl_results[[1]] <- people;
tree_results[[1]] <- trees;

t <- 1;

while(t <= years){
  
  felled <- felling(
    people = people, 
    trees = trees, 
    value = value,
    management_cost = management_cost,
    travel_cost = travel_cost,
    mobility = mobility); # removing trees from the landscape and adding value to people
  
  people <- felled[[2]];
  ppl_results[[t+1]] <- people; # stores results
  trees <- felled[[1]];
  
  if(length(matrix(trees, ncol = 4)[,1]) == 0){
    tree_results[[t+1]] <- NULL
    break;
  } else {
    
    r_l <- rate(trees, r,k) # calculates logistic growth rate based on carrying capacity
    if (r_l > 0) {
      trees <- growth(trees, r_l, maturity, max_age); # grows the forest
    }
    tree_results[[t+1]] <- trees;
  }
  t <- t + 1; # onto the next year!
}
return(list(ppl_results,tree_results));
