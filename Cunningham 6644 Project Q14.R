rm(list = ls())
set.seed(1)

#create a function to simulate an n-sided die roll
die_roll <- function(n) {
  sample(c(1:n),size = 1,prob = rep(1/n,n))
}

#create an empty list to store the expected value of number of turns
#for each replication of the game simulation
x <- list()

#create a function to simulate one replication of the coin game
game <- function(A_coins, B_coins, pot_coins) {
  #initialize value of number of turns at 0
  nturns <- 0
  #define the die roll values that result in game end
  end_roll <- c(4,5,6)
  
  while (TRUE) {
    resultA <- die_roll(6)
    
    #break loop if Player A's turn ends the game
    if(A_coins == 0 & resultA %in% end_roll){
      nturns <- nturns + 1
      break
    }
    
    #perform required action based on the die roll of Player A
      if(resultA %in% end_roll) {
        (A_coins <- A_coins -1) 
        (pot_coins <- pot_coins +1)}
      if(resultA == 2) {
        (A_coins <- A_coins + pot_coins) 
        pot_coins <- 0}
      if(resultA == 3) {
        (A_coins <- A_coins + floor(pot_coins*.5)) 
        (pot_coins <- pot_coins - floor(pot_coins*.5))}
    
    resultB <- die_roll(6)
    
    #break the loop if Player B's turn ends the game
    if(B_coins == 0 & resultB %in% end_roll) {
      nturns <- nturns + 1
      break
    }
    
    #perform required action based on the die roll of Player B
      if(resultB %in% end_roll) {
        (B_coins <- B_coins -1) 
        (pot_coins <- pot_coins +1)}
      if(resultB == 2) {
        (B_coins <- B_coins + pot_coins) 
        pot_coins <- 0}
      if(resultB == 3) {
        (B_coins <- B_coins + floor(pot_coins*.5)) 
        (pot_coins <- pot_coins - floor(pot_coins*.5))}
    nturns <- as.integer(nturns +1)
  }
  return(nturns)
}

#run one replication of the game function to ensure
#output looks as expected
game(4,4,2)

nrep <- 100000
#run 100,000 replications of the game and store results in list x
for (i in 1:nrep){
  x[i] <- game(4,4,2)
}

#unlist list x to analyze its values
replications <- unlist(x)

#plot a histogram of the expected value of number of turns of each replication
#of the game function
hist(replications, breaks = 200, col = "lightblue", 
     main = "Histogram of 1st - 3rd Quartile of Number of Cycles of Coin Game", 
     xlab = "Number of Cycles of Coin Game", xlim = c(5,23))

?hist

#print the summary quartile, min, and max values of the replications
summary(replications)

find_mode <- function(a) {
  u <- unique(a)
  tab <- tabulate(match(a, u))
  u[tab == max(tab)]
}

find_mode(replications)

#write the unlisted replication values to a csv file 
#in order to input into Arena
write.csv(replications, file = "data_distribution.csv")

sample_mean <- mean(replications)
sample_var <- var(replications)
t_value <- qt(.01, nrep - 1, lower.tail = FALSE)

lower_bound <- sample_mean - t_value * sqrt(sample_var)/sqrt(nrep)
lower_bound
upper_bound <- sample_mean + t_value * sqrt(sample_var)/sqrt(nrep)
upper_bound
sample_mean
upper_bound - sample_mean


