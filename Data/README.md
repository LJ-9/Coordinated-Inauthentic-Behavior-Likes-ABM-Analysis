The files `ABM_and_Classification.R` and [auxiliary functions](https://github.com/LJ-9/Coordinated-Inauthentic-Behavior-Likes-ABM-Analysis/tree/main/ABM_Classification/auxiliary%20functions) in [/ABM_Classification](https://github.com/LJ-9/Coordinated-Inauthentic-Behavior-Likes-ABM-Analysis/tree/main/ABM_Classification) directory generate a file named `ABM Data.Rda` placed in this directory. 

`ABM Data.Rda` contains the ABM data analysed. Data exploration included 4 parameters.


/single_data_ABM contains non-appended ABM vote data. These files are appended into `ABM Data.Rda` later (through script `/ABM_Classification/ABM_and_Classification.R`). Hence, the single files can be deleted (there are 100,000 (100 runs x 1,000 voting rounds in each run), each containing a voting round of all agents (1,900) per parameter combination).


`ABM Data.Rda` is a 400,000 x 1913 dataframe (that should load rather quickly) and is structured as follows:
- h_l: probability with which property 1 is sampled in round: d_1
- u_d: probability with which property 2 is sampled in round: d_2
- y_n: probability with which property 3 is sampled in round: d_3
- y_n_p: probability with which property 3 is sampled in round, individually for each agent: d_a
- n_agents: number of agents: 1900, first 1000 genuine agents, then 900 nefarious of each type (cf. section 2.2 in paper)in the following order: A_up, A_down, A_both, D_down, D_up, D_both, L_down, L_up, L_both
- pri_perc_comp_lower: lower bound of Ca(p2) = Ca(p3), from which competences are sampled for each agents each round uniformly
- pri_perc_comp_upper: upper bound of Ca(p2) = Ca(p3), from which competences are sampled for each agents each round uniformly
- pub_perc_comp_lower: lower bound of Ca(p1) from which competences are sampled for each agents each round uniformly
- pub_perc_comp_upper: upper bound of Ca(p1) from which competences are sampled for each agents each round uniformly
- run: seed/run log, 1-100
- sig_pub: sampled p1
- sig_y_n: sampled p2
- columns 13-1913: votes of agents 1-1900, in the above mentioned order.
- there is 400000 rows: the model explored 4 parameter combinations each run in 100 runs, each of which again featured 1000 voting rounds.




[/classification_results](https://github.com/LJ-9/Coordinated-Inauthentic-Behavior-Likes-ABM-Analysis/tree/main/Data/classification_results) contain the classification results of the ABM data for different populations looked at: 
	
- /classification_results/P_A_D_L_up contains analysis of sub populations P_A_up, P_D_up, P_L_up: there should be 900 files: 100 runs analysed, per 3 parameter 		combinations (focus as in paper) and per 3 populations.
	
- /classification_results/P_all contains analysis of population P_all: there should be 1600 files: 100 runs analysed, 4 parameter combination (full exploration), 4 voting round sizes
	
- /classification_results/P_large_pop_robust contains analysis of large population P_all with 1000 genuine agents: there should be 1600 files: 100 runs analysed, 4 parameter combination (full exploration), 4 voting round sizes
	
	- each analysis file named after parameters, run number, number of genuine agents, or agent groups, basis for calculating mean/SD of misclassification
	
	- the files contain list "conf", per run, per parameter combination, per round size (and per number of genuine agents) are structured as follows:

		- §outpar$km_lr: confusion matrix of classification given k-means clustering
		- §outpar$gmm_lr: confusion matrix of classification given GMM clustering
		- §outpar$pars: logged parameter combinations, cf. variable names like in ABM Data.Rda
		- §outpar$lasso_km_yes: if 1 in all 5 bootstrapped datasets, then lasso regularization was used after k-means clustering (always the case usually)
		- [if present] $out_par$km_entr: confusion matrix, entropy based classification and identification measures based on k-means clustering, used in data exploration
		- [if present] $out_par$gmm_entr: confusion matrix, entropy based classification and identification measures based on GMM clustering, used in data exploration
		- $outpar$lasso_gmm_yes: as above
		- $outpar$gap_fail: if gap statistic did not pick cluster >= 2, then 2 was imposed
		- [if present] $out_par$gap_fail_entr: entropy based classification and identification measures based on entropy clustering, used in data exploration
		- $outpar$classification_km_lr: how the data was eventually classified: 1= classified as genuine, 2 = classified as nefarious given k-means algorithm
		- $outpar$classification_gmm_lr: how the data was eventually classified: 1= classified as genuine, 2 = classified as nefarious given GMM algorithm
		- [if present] $out_par$classification_km_entr: entropy based classification and identification measures based on k-means clustering, used in data exploration
		- [if present] $out_par$classification_gmm_entr: entropy based classification and identification measures based on GMM clustering, used in data exploration
	
		- $warning: warnings logs
		- $error: error logs




The [classification result files](https://github.com/LJ-9/Coordinated-Inauthentic-Behavior-Likes-ABM-Analysis/tree/main/Data/classification_results) this folder eventually contains. These files contain mean and SD of misclassifications across classifier methods and populations, used for MCS calculations), generated by:

1. `/ABM_Classification/Misclassification_small_populations_P_A_up_P_D_up_P_L_up.R`, generate manually with chooser for which subpopulation to look at in given file.

	- 	`group_9_mean_100_gen_agents_gmm.Rda`
	- `group_9_mean_100_gen_agents_km.Rda`
	- `group_9_sd_100_gen_agents_gmm.Rda`
 	- `group_9_sd_100_gen_agents_km.Rda`

 	- `group_2_mean_100_gen_agents_gmm.Rda`
 	- `group_2_mean_100_gen_agents_km.Rda`
 	- `group_2_sd_100_gen_agents_gmm.Rda`     
 	- `group_2_sd_100_gen_agents_km.Rda`      
 
 	- `group_6_mean_100_gen_agents_gmm.Rda`  
	- `group_6_mean_100_gen_agents_km.Rda`    
 	- `group_6_sd_100_gen_agents_gmm.Rda`
 	- `group_6_sd_100_gen_agents_km.Rda`  

- structured as follows: 
list named "sdev_kmm" of length 3 in e.g. `group_2_sd_100_gen_agents.Rda`: 		
	- sdev_km[[1]]: standard deviation of misclassification share among all agents groups in Parameter combination 1 in P_A_up (group 2).
	- sdev_km[[1]]$mis_1_km_lr: 0.0, SD of share of misclassified genuine agents as nefarious in parameter 1 based on KM classification
	- sdev_km[[1]]$mis_2_km_lr: 0, SD of share of misclassified upvote amplifier A_up agents as genuine in parameter 1
	- sdev_km[[2]] - parameter 2
	- sdev_km[[3]] - parameter 3
	
	Cf. notes above for order of agent types/groups `ABM Data.Rda` or main script `ABM_and_Classification.R` -> groups

 
2. `/ABM_Classification/Misclassification_P_all.R`, 
	- `mean_100_gen_agents_gmm.Rda`
 	- `mean_100_gen_agents_km.Rda`
 	- `sd_100_gen_agents_gmm.Rda`
 	- `sd_100_gen_agents_km.Rda`

- structured as follows, list named "mean_gmm" of length 3 in e.g. `mean_100_gen_agents_gmm.Rda`
	- mean_gmm[[1]]: misclassification share among all agents groups in Parameter combination 1
	- mean_gmm[[1]]$mis_1_gmm_lr: 0.0432, mean share of misclassified genuine agents as nefarious in parameter 1 based on GMM classification
	- mean_gmm[[1]]$mis_6_gmm_lr: 0, mean share of misclassified upvote distorter D_up agents as genuine in parameter 1
	- mean_gmm[[2]] - parameter 2
	- mean_gmm[[3]] - parameter 3. 
	
	Cf. notes above for order of agent types/groups `ABM Data.Rda` or main script `ABM_and_Classification.R` -> groups

And in confusion matrices format (2x2, true genuine, classified as genuine, true nefarious, classified as nefarious), per parameter combination (1-3):

 - `mean_stddev_1_500rounds_100_gen.Rda`
-  `mean_stddev_2_500rounds_100_gen.Rda`
-  `mean_stddev_3_500rounds_100_gen.Rda`

- structured as follows: 
	- perform[[1]]$km_mean: Mean confusion matrix, based on k-means classification
	- perform[[1]]$km_sd: SD of confusion matrix, based on k-means classification
	- perform[[1]]$gmm_mean: Mean confusion matrix, based on GMM classification
	- perform[[1]]$gmm_sd: SD of confusion matrix, based on GMM classification

	- perform[[1]]$km_entr_mean: Mean confusion matrix, based on k-means classification and entropy measures, only used in data exploration
	- perform[[1]]$km_entr_sd: SD confusion matrix, based on k-means classification and entropy measures, only used in data exploration
	- perform[[1]]$gmm_entr_mean: Mean confusion matrix, based on GMM classification and entropy measures, only used in data exploration
	- perform[[1]]$gmm_entr_sd: SD confusion matrix, based on GMM classification and entropy measures, only used in data exploration

	- perform[[1]]$pars: logged parameters, cf. ABM Data.Rda

	- perform[[2]]: classification raw data


3. `/ABM_Classification/Misclassification_P_all_robustness.R`, 
  
- does what 2, does, and results are structured in the same way. The script is meant as an optional code file, from which you can generate robustness results with varying voting rounds (250, 750, 1000 in addition to the default 500 set in (2)) and misclassification results split by agent group (exactly like from (2)), outputs variants of files named, e.g.: 

 	- `mean_100_gen_agents_gmm_1000_rounds_robustness_res.Rda`
	- `mean_100_gen_agents_km_1000_rounds_robustness_res.Rda`
 	- `sd_100_gen_agents_gmm_1000_rounds_robustness_res.Rda`
 	- `sd_100_gen_agents_km_1000_rounds_robustness_res.Rda`

	- `mean_stddev_1_250rounds_100_gen_robustness_res.Rda`
 	- `mean_stddev_2_250rounds_100_gen_robustness_res.Rda`
	- `mean_stddev_3_250rounds_100_gen_robustness_res.Rda`




4. `/ABM_Classification/Misclassification_large_population_robustness.R`,

- 	Lastly: this directory may also include mean/SD results given robustness checks relating to increasing both the number of genuine agents in the population and changing voting rounds, generated by: `/ABM_Classification/Misclassification_large_population_robustness.R`, in which both voting rounds and number of genuine agents can be adapted, e.g. 
 "mean_stddev_1_750all_1000_gen_robustness_res.Rda" -Parameter combination 1, 750 voting rounds, 1000 genuine agents, containing list "perform", structured as in 2. above.
 
