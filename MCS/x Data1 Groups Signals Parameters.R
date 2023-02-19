# agents
group_1 <- c(1:1000);     # 1 genuine agents
group_2 <- c(1001:1100)   # 2 Puppets - U - Consp, c1
group_3 <- c(1101:1200)   # 3 Puppets - D - Consp, c2
group_4 <- c(1201:1300)   # 4 Puppets - Full Consp -Parallel Universe, c3
group_5 <- c(1301:1400)   # 5 Puppets - AQ - High - Coordinating, cd1
group_6 <- c(1401:1500)   # 6 Puppets - AQ - Low - Coordinating, cd2
group_7 <- c(1501:1600)   # 7 Puppets - AQ - Always - Coordinating, cd3
group_8 <- c(1601:1700)   # 8 Puppets - Lone Wolf - High - Non-Coordinating, id1
group_9 <- c(1701:1800)   # 9 Puppets - Lone Wolf - Low - Non-Coordinating, id2
group_10 <- c(1801:1900)  # 10 Puppets - Lone Wolf - Always - Non-Coordinating, id3

# signal vectors
signal_1<-c('high','low')
signal_2<-c('U','D')
signal_3<-c('Y','N')


parlist_prep = list(h_l = c(.75),                     # sample high with probability 75%, and low with 1-75% = 25%
                    u_d = c(.75,.5,.1),               # sample up with probability 75/50/10%, and down with 1-75/50/10% = 25/50/90%
                    y_n = c(.9,.5,.1),                # sample yes with probability 90/50/10%, and no with 1-90/50/10% = 10/50/90%
                    y_n_p = c(.9,.5,.1),              # sample yes_private with probability 90/50/10%, and no_private with 1-90/50/10% = 10/50/90%
                    n_agents = c(1900),                # number of agents
                    pri_perc_comp_lower = c(75,100),   # drawing perception competence for biased agents uniformely from interval [1,1] or [0.75,0.95]
                    pri_perc_comp_upper = c(95,100),
                    pub_perc_comp_lower = c(65),     # drawing perception competence for public high/low signal uniformely from interval [0.65,0.95]
                    pub_perc_comp_upper = c(95))

# creates dataframe from all combinations of parameter values
parlist_all=expand.grid(parlist_prep, stringsAsFactors = FALSE)

par_low <- subset(parlist_all, parlist_all$h_l == .75 & parlist_all$u_d ==.75 & parlist_all$y_n == .9 & parlist_all$y_n_p == .9 & parlist_all$pri_perc_comp_lower==100 & parlist_all$pri_perc_comp_upper==100 & parlist_all$pub_perc_comp_lower==65 & parlist_all$pub_perc_comp_upper==95)
par_med <- subset(parlist_all, parlist_all$h_l == .75 & parlist_all$u_d ==.5 & parlist_all$y_n == .5 & parlist_all$y_n_p == .5 & parlist_all$pri_perc_comp_lower==100 & parlist_all$pri_perc_comp_upper==100 & parlist_all$pub_perc_comp_lower==65 & parlist_all$pub_perc_comp_upper==95)
par_high <- subset(parlist_all, parlist_all$h_l == .75 & parlist_all$u_d ==.1 & parlist_all$y_n == .1 & parlist_all$y_n_p == .1 & parlist_all$pri_perc_comp_lower==100 & parlist_all$pri_perc_comp_upper==100 & parlist_all$pub_perc_comp_lower==65 & parlist_all$pub_perc_comp_upper==95)
par_high_asterix <- subset(parlist_all, parlist_all$h_l == .75 & parlist_all$u_d ==.1 & parlist_all$y_n == .1 & parlist_all$y_n_p == .1 & parlist_all$pri_perc_comp_lower==75 & parlist_all$pri_perc_comp_upper==95 & parlist_all$pub_perc_comp_lower==65 & parlist_all$pub_perc_comp_upper==95)

parlist = rbind(par_low,par_med,par_high, par_high_asterix)