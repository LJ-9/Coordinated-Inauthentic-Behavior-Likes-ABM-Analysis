# this function generates the voting data for 1 round and stores/returns it along with the parameter combination, the seed and the signals in a list

votes <- function(parlist,l,group_1, group_2 , group_3 ,group_4 ,group_5 ,group_6 ,group_7 ,group_8 ,group_9 ,group_10 ,signal_1, signal_2,signal_3){ #n_agents, p_corr, i_high, i_low,comp_bia) {

  # learning: sample is faster than runif and both return uniform distr if probabilities in sample are not specified
  
  # sample c_g - public perception comptence for all agents to assess the public signal high/low, i.e. "genuine" competence (c_g)
  
  c_g<-sample((parlist$pub_perc_comp_lower:parlist$pub_perc_comp_upper/100),parlist$n_agents,replace=TRUE) 
  
  # sample c_b - private perception competence level for biased agents, i.e. puppet competence (c_p), for simplicit, just sample for all
  
  c_p<-sample((parlist$pri_perc_comp_lower:parlist$pri_perc_comp_upper/100),parlist$n_agents,replace=TRUE) 
  
  # sample public signal high/low
  
  sig_pub<-sample(signal_1, 1, replace=TRUE, prob=c(parlist$h_l,1-parlist$h_l))
  
  # sample private up/down signal
  
  sig_u_d<-sample(signal_2, 1, replace=TRUE, prob=c(parlist$u_d,1-parlist$u_d))
  
  # sample private yes/no signal 
  
  sig_y_n<-sample(signal_3, 1, replace=TRUE, prob=c(parlist$y_n,1-parlist$y_n))
  
  
  # sample individual (!) private yes/no signal, for simplicity, just sample for all. 
  
  sig_y_n_p<-sample(signal_3, parlist$n_agents, replace=TRUE, prob=c(parlist$y_n,1-parlist$y_n))
  
  # create voting vector
  
  vote <- matrix(ncol=parlist$n_agents)#n_agents)
  
  # create  vectors for individual signal perceptions
  sig_pub_agent <- c() # P1
  sig_u_d_agent <- c() # P2 
  sig_y_n_agent <- c() # P3
  sig_y_n_p_agent <- c() # P3, individually sampled
  
  


  #### perception functions ### for simplicity, sampled for all agents (for simplicity, the private perception competence is of course never invoked on the genuine agents)
  for (i in 1:parlist$n_agents){
    
    #### public signal perception function signal_1 = c("high","low")
   
     ## perception functcin  P1 public signal h/l sig_pub 
    
    if (sig_pub == signal_1[1]) { # if high in the real world
    sig_pub_agent[i] = sample(signal_1,1, replace= TRUE, prob=c(c_g[i], 1-c_g[i])) # each agent is rather likely with competence c_g to perceive that there is a high signal, if there is a hgih signal in the real world
    } else { sig_pub_agent[i] = sample(signal_1,1, replace= TRUE, prob=c(1-c_g[i], c_g[i]))} # each agent is rather likely to perceive a low signal, if there is a low signal in the real world
  
    
    ### private signals perception functions P2, P3
    
    ## perception function private P2 u/d sig_u_d, relevant for group_2,group_3,group_4
    
    if (sig_u_d == signal_2[1]) { # if U in real world
    sig_u_d_agent[i] = sample(signal_2,1, replace= TRUE, prob=c(c_p[i], 1-c_p[i])) # each agent is rather likely with competence c_p to perceive that there is a U signal, if there is a U signal in the real world
    } else { sig_u_d_agent[i] = sample(signal_2,1, replace= TRUE, prob=c(1-c_p[i], c_p[i]))} # each agent is rather likely to perceive a D signal, if there is a D signal in the real world

    ## perception function private P3 y/n sig_y_n, relevant for group_5,group_6,group_7
    
    if (sig_y_n == signal_3[1]) { # if Y in real world
      sig_y_n_agent[i] = sample(signal_3,1, replace= TRUE, prob=c(c_p[i], 1-c_p[i])) # each agent is rather likely with competence c_p to perceive that there is a Y signal, if there is a Y signal in the real world
    } else { sig_y_n_agent[i] = sample(signal_3,1, replace= TRUE, prob=c(1-c_p[i], c_p[i]))} # each agent is rather likely to perceive a N signal, if there is a N signal in the real world
    
    ## perception function private AND PERSONAL/INDIVDUAL P3 y_p/n_p sign_y_n_p, relevant for group_8,group_9,group_10
    
    if (sig_y_n_p[i] == signal_3[1]) { # if Y_p in real world per agent
      sig_y_n_p_agent[i] = sample(signal_3,1, replace= TRUE, prob=c(c_p[i], 1-c_p[i])) # each agent is rather likely with competence c_p to perceive that there is a personal Y signal, if there is a personal Y signal in the real world
    } else { sig_y_n_p_agent[i] = sample(signal_3,1, replace= TRUE, prob=c(1-c_p[i], c_p[i]))} # each agent is rather likely to perceive a personal N signal, if there is a personal N signal in the real world
 
    
  }
  
  
  
  # genuine agents vote 
      # genuine agents group 1 vote: each agent has a perception function whether one identifies the signal correctly, refelcted in c_g
  for (i in group_1){
    if (sig_pub_agent[i] == signal_1[1]) {
      vote[,i] = 1 } else { vote[,i] = 0}
    } 
 
  

  #######################
  
  # nefarious agents vote

      # nefarious agents group 2 vote: *A_upvote*

for (i in group_2){
  if (sig_u_d_agent[i] == signal_2[1]) { # if U, vote 1
    vote[,i] = 1 } else { # else, vote like genuine agents dependent how you perceived the public signal, stored in sig_pub_agent
      if (sig_pub_agent[i] == signal_1[1]) {
        vote[,i] = 1 } else { vote[,i] = 0}
    } # end if
} # end for
  

    
    
      # nefarious agents group 3 vote *A_downvote*

for (i in group_3){
  if (sig_u_d_agent[i] == signal_2[2]) { # if D, vote 0
    vote[,i] = 0 } else { # else, vote like genuine agents dependent how you perceived the public signal, stored in sig_pub_agent
      if (sig_pub_agent[i] == signal_1[1]) {
        vote[,i] = 1 } else { vote[,i] = 0}
    } # end if
} # end for
    
    
    
      # nefarious agents group 4 vote *A_both*
for (i in group_4){
  if (sig_u_d_agent[i] == signal_2[1]) { # if U, vote 1
    vote[,i] = 1 } else { vote[,i] = 0}  # if D, vote 0
} # end for

      
     
      #######################

     
     
      # nefarious agents group 5 vote *D_downvote*

for (i in group_5){
  if (sig_y_n_agent[i] == signal_3[1] & sig_pub_agent[i] == signal_1[1]) { # if perceived high signal AND perceived Y signal
    vote[,i] = 0 } else { # else, vote like genuine agents dependent how you perceived the public signal, stored in sig_pub_agent
      if (sig_pub_agent[i] == signal_1[1]) {
        vote[,i] = 1 } else { vote[,i] = 0}
    } # end if
} # end for


      # nefarious agents group 6 vote *D_upvote*

for (i in group_6){
   if (sig_y_n_agent[i] == signal_3[1] & sig_pub_agent[i] == signal_1[2]) { # if perceived low signal AND perceived Y signal
   vote[,i] = 1 } else { # else, vote like genuine agents dependent how you perceived the public signal, stored in sig_pub_agent
      if (sig_pub_agent[i] == signal_1[1]) {
        vote[,i] = 1 } else { vote[,i] = 0}
    } # end if
} # end for

     
     
     
      # nefarious agents group 7 vote *D_both
for (i in group_7){
  if (sig_y_n_agent[i] == signal_3[1] & sig_pub_agent[i] == signal_1[1]) { # if perceived high signal AND perceived Y signal
    vote[,i] = 0 } else { 
      if (sig_y_n_agent[i] == signal_3[1] & sig_pub_agent[i] == signal_1[2]) { # if perceived high signal AND perceived Y signal
        vote[,i] = 1 } else# else, vote like genuine agents dependent how you perceived the public signal, stored in sig_pub_agent
      if (sig_pub_agent[i] == signal_1[1]) {
        vote[,i] = 1 } else { vote[,i] = 0}
    } # end if
} # end for

     
     
      # nefarious agents group 8 *L_downvote*

for (i in group_8){
  if (sig_y_n_p_agent[i] == signal_3[1] & sig_pub_agent[i] == signal_1[1]) { # # if  perceived high signal AND perceived private Y signal
    vote[,i] = 0 } else { 
      if (sig_pub_agent[i] == signal_1[1]) {
        vote[,i] = 1 } else { vote[,i] = 0}
    } # end if
} # end for
    
     
     
     
      # nefarious agents group 9 *D_upvote*
for (i in group_9){
  if (sig_y_n_p_agent[i] == signal_3[1] & sig_pub_agent[i] == signal_1[2]) { # # if perceived low signal AND perceived private Y signal
    vote[,i] = 1 } else { 
          if (sig_pub_agent[i] == signal_1[1]) {
            vote[,i] = 1 } else { vote[,i] = 0}
    } # end if
} # end for

     
     
      # nefarious agents group 10 vote *L_both*


for (i in group_10){
  if (sig_y_n_p_agent[i] == signal_3[1] & sig_pub_agent[i] == signal_1[1]) { # # like group 8, if perceived high signal AND perceived private Y signal
    vote[,i] = 0 } else { 
      if (sig_y_n_p_agent[i] == signal_3[1] & sig_pub_agent[i] == signal_1[2]) { # # like group 9, if perceived low signal AND perceived private Y signal
        vote[,i] = 1 } else# else, vote like genuine agents dependent how you perceived the public signal, stored in sig_pub_agent
          if (sig_pub_agent[i] == signal_1[1]) {
            vote[,i] = 1 } else { vote[,i] = 0}
    } # end if
} # end for





  
  # store voting profiles, corresponding signals and parameters
  
  #dat <- list(parameter = c(parlist$p_corr, parlist$comp_bia, parlist$i_high, parlist$i_low, parlist$n_agents), votings = vote, signal = c(sig_pub, sig_pri))
  dat <- list(h_l = c(parlist$h_l), #P1
              u_d = c(parlist$u_d), #P2
              y_n =c(parlist$y_n), #P3
              y_n_p =c(parlist$y_n_p), #P3 individual
              n_agents = c(parlist$n_agents), #number of agents
              pri_perc_comp_lower = c(parlist$pri_perc_comp_lower), #competence nefarious, lower bound
              pri_perc_comp_upper = c(parlist$pri_perc_comp_upper), #competence nefarious, upper bound
              pub_perc_comp_lower = c(parlist$pub_perc_comp_lower), #competence genuine, lower bound
              pub_perc_comp_upper = c(parlist$pub_perc_comp_upper), #competence genuine, upper bound
              run = l, #log run number
              signal = c(sig_pub, sig_u_d, sig_y_n), # log sampled properties P1,P2,P3
              votings = vote ) # document votes 
  
  return(dat) 
  } # end produce simulation data
