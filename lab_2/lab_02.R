
name = 'Johannes Hedström'

liuid = 'johed883'





# lab 2 


#  1.1.1 sheldon game

sheldon_game <- function(player1, player2){
  # Making a list of the rules
  rules = list('rock' = c('scissors', 'lizard'),
               'paper' =  c('rock', "spock"),
               'scissors' = c('paper', 'lizard'),
               'lizard' = c('spock', 'paper'),
               'spock' = c('scissors', 'rock'))
  
  # Making sure the input is lowercase and character
  player1 <- tolower(as.character(player1))
  player2 <- tolower(as.character(player2))
  
  # Checking the input for the players and stopping if the input is incorrect 
  stopifnot(player1 %in% c('rock', 'paper', 'scissors', 'lizard', 'spock' ),player2 %in% c(c('rock', 'paper', 'scissors', 'lizard', 'spock' )))  
  # Checking if player 2 are in any of the rules for the play of player 1
  if (player2 %in% rules[[player1]]){
    return('Player 1 wins!')
    
    # Checking if player 1 are in any of the rules for the play of player 2
  } else if (player1 %in% rules[[player2]]){
    return('Player 2 wins!')
   
  }
    else { # DRAW! If none of the rules are TRUE to the player picks
      return('Draw!')
  
  }
  
}



#  1.2.1 my_moving_median


my_moving_median <- function(x,n,...){
  # Checking the input to be a vector and a scalar with a value bigger than 0
  stopifnot(is.vector(x), is.numeric(n) && length(n) == 1 && n > 0)
 
  y <- vector()
  # Looping over the length of the vector -n
  for (t in 1:(length(x)-n)){
    upper = t+n #  t + n 
    y[t] <- median(x[t:upper], ...) # Median value
  }
  
  return(y)
}




#  1.2.2 for_mult_table



for_mult_table <- function(from,to){
  # checking the inputs
  try((!is.integer(from) && from <= 0 && !is.integer(to) && to <= 0),stop('The arguments is not numerical scalars'))
  
  # Creating the empty matrix
  r_c <- to - from + 1 # nr of rows and columns
  mat <-matrix(nrow=r_c, ncol=r_c)
  from_to <- from:to # vector from : to
  colnames(mat) <- from_to  # giving it the col and row names
  rownames(mat) <-  from_to
  
  
  for (i in 1:r_c) { # looping over every row in the matrix, doing the multiplication and adding it to the matrix
    for (t in 1:r_c){
    mat[i,t] <- from_to[i]*from_to[t]
  }}
  return(mat)
  
}



# 1.2.3 cor_matrix *

cor_matrix <- function(X){
  # checking the input is data frame and numerical
  stopifnot(all(sapply(X, function(x) is.numeric(x))) && is.data.frame(X))
  
  # Taking the means for each variable
  means <- colMeans(X)
  sdev <- c()
  # Calculating all standard deviations
  for (i in 1:ncol(X)){
  sdev[i] <- sqrt((sum((X[,i] - means[i])^2))/(nrow(X)))
  }
  # loop to calculate the correlation and put it in the cor_matrix
  cor_mat <- matrix(nrow=ncol(X),ncol=ncol(X))
  for(k in 1:ncol(X)){
    for(t in 1:ncol(X)){
      cor_mat[k,t] <- round((mean((X[,t]-means[t])*(X[,k] - means[k])))/(sdev[t] * sdev[k]),5) # rounding the correlations to 5 decimals
    
    }}
  return(cor_mat)
}





#  1.3.1 find_cumsum


find_cumsum <- function(x, find_sum){
  # cheking the input
  stopifnot(is.vector(x) && is.numeric(x),is.numeric(find_sum))
  
  # Creating the while loop and 
  i = 1
  s = 0
  while(s < find_sum) { # While the cumsum is smaller than find_sum 
    s <- s + x[i] # cumulative sum 
    i = i + 1  # to iterate through vector x in the loop
    if (i > length(x)) # Stop the loop if all the values in the vector is calculated
      break
  }
  return(s)
} 


#  1.3.2 while_mult_table

while_mult_table <- function(from,to){
  
  # checking the inputs
  try((!is.integer(from) && from <= 0 && !is.integer(to) && to <= 0),stop('The arguments is not numerical scalars'))

  # Creating the empty matrix
  r_c <- to - from + 1 # nr of rows and columns
  mat <-matrix(nrow=r_c, ncol=r_c)
  from_to <- from:to # vector from : to
  colnames(mat) <- from_to  # giving it the col and row names
  rownames(mat) <-  from_to
  
  i = 1
  while (i <= r_c) { # Looping the rows
      t = 1 # resetting t to 1 for every value of i
   while (t <= r_c){# looping the columns
     mat[i,t] <- from_to[i]*from_to[t]
      t = t + 1
      }
    i = i + 1}
  return(mat)

}

# 1.3.3 trial division factorization *

# For this following code in 1.3.3 i got inspiration from the Wikipedia page linked in the assignment which had:
# https://en.wikipedia.org/wiki/Trial_division

trial_division_factorization <- function(x){
  # input check 
  stopifnot(is.integer(as.integer(x)))
  # vector to fill with prime values
  prime <- c()
  i <- 1
  while (x %% 2 == 0){ # while x modulus 2 = 0, 2 gets a place in the list and x is divided by 2
    prime[i] <- 2
    i <- i + 1
    x <- x / 2 # Divide x by 2 for this iteration
  }
  j  <- 3 # Setting j to 3 and for every other value up to the square root of x i try to find the least dividable number of x
  while(j * j <= x){
    if (x %% j == 0){
      prime[i] <- j # if x modulus j == 0 then j gets added to the vector
      x <- x / j # Divide x by j for this iteration
      i <- i + 1 
    } else {
      j <- j+2
    }
  if (x != 1)
    prime[i] <- x # least prime value >2 left to add in the list
  }
  return(prime)
}





#  1.4.1 repeat_find_cumsum

repeat_find_cumsum <- function(x, find_sum){
  # cheking the input
  stopifnot(is.vector(x) && is.numeric(x),is.numeric(find_sum))
  
  # Creating the while loop and 
  i = 1
  s = 0
  repeat{ # repeting this loop until the conditions in if
    s <- s + x[i] # cumulative sum 
    i = i + 1  # to iterate through vector x in the loop
    if (s >= find_sum || i > length(x)) # Stop the loop if all the values in the vector is calculated or the cumsum is >= find_sum
      break
  }
  return(s)
} 




#  1.4.2 repeat_my_moving_median

repeat_my_moving_median <- function(x,n,...){
  # Checking the input to be a vector and a scalar with a value bigger than 0
  stopifnot(is.vector(x), is.numeric(n) && length(n) == 1 && n > 0)
  
  y <- vector()
  # Looping over the length of the vector - n
  t = 1
  repeat{
    upper = t+n #  t + n 
    y[t] <- median(x[t:upper],...) # Median value
    t = t+1
    if(t > length(x)-n) # If t is greater than length of x -n then stop the loop
      break
  }
  
  return(y)
}





#  1.5.1 in_environment



in_environment <- function(env){
  stopifnot(is.character(env) | is.environment(env)) # checking the input, either character or environment
  
  t <- ls(env) # return a vector of strings with the names of objects in the environment
  return(t)
}



#  1.5.2 where *

where <- function(fun){
  # checking the input 
  stopifnot(length(fun)==1 && is.character(fun))
  
  # All the environments
  text <- search()
  
  i=1 # While loop that goes through all environments to check for fun
  while(i <= length(text)){
    
    env_text <- ls(text[i])
    if (fun %in% env_text){ # If fun is in the environment 
      return(text[i])
      break # stopping the loop
    }
    if(i == length(text)){ # if the function is not found in any environment
      cat('"',fun, ' not found','!"', sep="") # printing the input name and that it´s not found
    }
    i <- i+1
  }
}



#  1.6.1 cov 

cov <- function(X){
  # checking that the input is a data frame and that the variables are numeric
  stopifnot(is.data.frame(X), all(sapply(X, function(x) is.numeric(x))))
  # using lapply and unlist to get a vector of the covariance with an anonymous function
  cov_vec <- unlist(lapply(X, function(i) sd(i)/mean(i)))
  return(cov_vec)
}




#  1.7.1 moment
moment <- function(i){
  # checking input is numerical
  stopifnot(is.numeric(i) && length(i) == 1)
  
  # Creating a child function so you can have 2 levels of parameters
  function(x){
    # checking the input is numerical  
    stopifnot(is.numeric(x))
    mean((x-mean(x))^i) # Central moment calculations
  }
}




# 1.7.2 mcmc counter factory*


mcmc_counter_factory <- function(burnin,thin){
  # cheking the input
  stopifnot(is.numeric(burnin) && burnin >=0,is.numeric(thin) && thin > 0)
  iteration <- 0
  store_sample <- FALSE
  samples <- 0
  
  counter <- function(){ # the counter
    iteration <<- iteration + 1 # plus one for every use
    # if iteration modulus burnin plus thin ==0 or burnin +thin the sample is stored
    if(iteration %% (burnin +thin) == (burnin +thin) ||iteration %% (burnin +thin) ==0  ){
    store_sample <<- TRUE
    samples <<- samples+1
    } else{ # if the iteration isnt saved
      store_sample <<- FALSE
    }
    l <- list(iteration,store_sample,samples) # create a list of the objects and returning it
    return(l)
  }
}



