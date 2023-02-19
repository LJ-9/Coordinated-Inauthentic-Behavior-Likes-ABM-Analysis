# This files contains two functions used across multiple files in the MCS folder

# Define a subsetting function to loop over seeds and parameters
datasubset <- function(k,j){ # k : seed, j : parameter
  dataset <- subset(all, h_l==parlist$h_l[j] & u_d==parlist$u_d[j] & y_n==parlist$y_n[j] & y_n_p==parlist$y_n_p[j] & pri_perc_comp_lower == parlist$pri_perc_comp_lower[j] & pri_perc_comp_upper == parlist$pri_perc_comp_upper[j] & pub_perc_comp_lower == parlist$pub_perc_comp_lower[j] & pub_perc_comp_upper == parlist$pub_perc_comp_upper[j] &  run == k)#s[k])
  dataset = dataset[,-(1:10)]  # Remove parameters
  signals = dataset[,1:3] # Store signals     1: pub, 2: pri_u_d, 3: pri_y_n
  votes = dataset[,-(1:3)]   # Remove signals from data structure
  votes = as.matrix(votes)
  mode(votes) = "numeric"
  votes[votes==0] <- -1
  signals_votes <- list("Signals" = signals, "Votes" = votes)
  return(signals_votes)
}

# Recast the public signal as 1,-1 for easy comparison with majority vote:
pub.as.number <- function(signals_votes, round){
  if(signals_votes$Signals[round,1] == "high")
  {number <- 1}
  if(signals_votes$Signals[round,1] == "low")
  {number <- -1}
  return(number)
}