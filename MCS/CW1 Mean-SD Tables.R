# This file contains the functions needed to generated tables with 
# Majority Correctness Scores and their standard deviations.

#1 dataset subsetting function and technicality
library(here)
source(here("MCS/x Shared Mean-SD.R")) #1 dataset subsetting function and technicality

# The first functions here are defined given a seed and parameter choice, 
# i.e., given a dataset.
# Later, we loop over seeds.




# 2. WAS THE ONE JURY RIGHT IN THIS ROUND?
# Make a TRUE FALSE vector of whether jury was correct for each round in range
majority.correctnesses <- function(signals_votes, rounds, jury){
  verdicts <- vector("logical")
  for (round in rounds) {
    verdicts[round] <- sign(sum(signals_votes$Votes[round,jury])) == pub.as.number(signals_votes, round)
  }
  verdicts <- verdicts[rounds]
  return(verdicts)
}


# 3. HOW OFTEN WAS THE ONE JURY RIGHT OVER RANGE OF ROUNDS?
# Percent correct verdicts for single jury over range of rounds
majority.percent.correct <- function(signals_votes, rounds, jury){
  correct <- sum(majority.correctnesses(signals_votes, rounds, jury))
  percent.correct <- (correct/length(rounds))*100
  return(percent.correct)
}


# 4. HOW OFTEN WHERE EACH OF THESE JURIES RIGHT OVER RANGE OF ROUNDS?
# Vector of percent correct for multiple juries over range of rounds (in one seed)
juries.percentages.oneseed <- function(signals_votes, rounds, named.juries){
  table <- as.data.frame(matrix(0, ncol = 0, nrow = 1))  
  for (named.jury in named.juries) {
    unnamed.jury <- unlist(named.jury, use.names=FALSE)     #make jury list into vector so we can apply next function
    percentages <- data.frame(c(majority.percent.correct(signals_votes, rounds, unnamed.jury)))  #get percentages as dataframe so we can cbind them on
    table <- cbind(table, percentages) # bind
    vector <- as.numeric(table[1,])
  }
  return(vector)
}


# Now we move to loop over seeds


# 5a. FOR EACH SEED, DO 4.; COLLECT IN MATRIX
# Populated a matrix with rows of seeds, columns of juries, cells of correct percentages
juries.percentages.matrix <- function(seeds, rounds, named.juries, param){
  matrix <- matrix(nrow = length(seeds), ncol = length(named.juries))
  for (seed in seeds){
    matrix[seed, ] <- juries.percentages.oneseed(datasubset(seed, param), rounds, named.juries)
  }
  return(matrix)
}


# 5b. MEAN AND SD
# We calculate means and sd, building a table of them:
correct.won.mean.sd.table <- function(seeds, rounds, named.juries, param){
  matrix <- juries.percentages.matrix(seeds, rounds, named.juries, param)
  mean.sd <- matrix(0, ncol = length(named.juries), nrow = 2)
  for (k in 1:length(named.juries)){
    mean.sd[1, k] <- mean(matrix[ ,k])
    mean.sd[2, k] <- sd(matrix[ ,k])
  } 
  rownames(mean.sd) <- c('mean', 'sd')
  colnames(mean.sd) <- names(named.juries)
  return(mean.sd)
}


#### NOW RESTRICTIONS

### RESTRICTION TO UP
## RESTRICT TO UP ROUNDS, else as 5a. 
# 5b1. FOR EACH SEED, DO 4.; COLLECT IN MATRIX
juries.percentages.matrix_given.up <- function(seeds, rounds, named.juries, param){
  matrix <- matrix(nrow = length(seeds), ncol = length(named.juries))
  for (seed in seeds){
    up.rounds <- which(datasubset(seed, param)$Signals[rounds,2] %in% "U")
    matrix[seed, ] <- juries.percentages.oneseed(datasubset(seed, param), up.rounds, named.juries)
  }
  return(matrix)
}


# 5b2. MEAN AND SD --- as 5a2, but restricted to up rounds
# Invoke 5b1, calculate means and sd, building a table of them:
correct.won.mean.sd.table_given.up <- function(seeds, rounds, named.juries, param){
  matrix <- juries.percentages.matrix_given.up(seeds, rounds, named.juries, param)
  mean.sd <- matrix(0, ncol = length(named.juries), nrow = 2)
  for (k in 1:length(named.juries)){
    mean.sd[1, k] <- mean(matrix[ ,k])
    mean.sd[2, k] <- sd(matrix[ ,k])
  } 
  rownames(mean.sd) <- c('mean', 'sd')
  colnames(mean.sd) <- names(named.juries)
  return(mean.sd)
}


### RESTRICTION TO YES
## RESTRICT TO YES ROUNDS, else as 5a. 
# 5b1. FOR EACH SEED, DO 4.; COLLECT IN MATRIX
juries.percentages.matrix_given.yes <- function(seeds, rounds, named.juries, param){
  matrix <- matrix(nrow = length(seeds), ncol = length(named.juries))
  for (seed in seeds){
    up.rounds <- which(datasubset(seed, param)$Signals[rounds,3] %in% "Y")
    matrix[seed, ] <- juries.percentages.oneseed(datasubset(seed, param), up.rounds, named.juries)
  }
  return(matrix)
}


# 5b2. MEAN AND SD --- as 5a2, but restricted to up rounds
# Invoke 5b1, calculate means and sd, building a table of them:
correct.won.mean.sd.table_given.yes <- function(seeds, rounds, named.juries, param){
  matrix <- juries.percentages.matrix_given.yes(seeds, rounds, named.juries, param)
  mean.sd <- matrix(0, ncol = length(named.juries), nrow = 2)
  for (k in 1:length(named.juries)){
    mean.sd[1, k] <- mean(matrix[ ,k])
    mean.sd[2, k] <- sd(matrix[ ,k])
  } 
  rownames(mean.sd) <- c('mean', 'sd')
  colnames(mean.sd) <- names(named.juries)
  return(mean.sd)
}


### RESTRICTION TO UP & YES

## RESTRICT TO UP&YES ROUNDS, else as 5a, 5b. 
# 5c1. FOR EACH SEED, DO 4.; COLLECT IN MATRIX
juries.percentages.matrix_given.up.and.yes <- function(seeds, rounds, named.juries, param){
  matrix <- matrix(nrow = length(seeds), ncol = length(named.juries))
  for (seed in seeds){
    up.rounds <- which(datasubset(seed, param)$Signals[rounds,2] %in% "U"  &
                         datasubset(seed, param)$Signals[rounds,3] %in% "Y")
    matrix[seed, ] <- juries.percentages.oneseed(datasubset(seed, param), up.rounds, named.juries)
  }
  return(matrix)
}


# 5c2. MEAN AND SD --- as 5a2, but restricted to up&yes rounds
# Invoke 5c1, calculate means and sd, building a table of them:
correct.won.mean.sd.table_given.yes.and.up <- function(seeds, rounds, named.juries, param){
  matrix <- juries.percentages.matrix_given.up.and.yes(seeds, rounds, named.juries, param)
  mean.sd <- matrix(0, ncol = length(named.juries), nrow = 2)
  for (k in 1:length(named.juries)){
    mean.sd[1, k] <- mean(matrix[ ,k])
    mean.sd[2, k] <- sd(matrix[ ,k])
  } 
  rownames(mean.sd) <- c('mean', 'sd')
  colnames(mean.sd) <- names(named.juries)
  return(mean.sd)
}
