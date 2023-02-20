### This folder "A_ABM_Classification" contains the following:

1. The main script (`ABM_and_Classification.R`) for invoking the ABM and the classification analysis that also saves the classification results that serve as input for 2. below. 

	4 parameter combinations are initiated in the main file, with the fourth serving further exploration purposes, while the first three are pivotal in the paper. The file calls many auxiliary functions that contain e.g. voting functions, dimensionality reduction steps, clustering, logistic regression, located in the folder: "auxiliary functions". Among the auxiliary functions, there are entropy computing functions that served data exploration purposes. The main script and auxiliary functions mention signals, which refer to properties described in the paper.



2. Scripts to calculate misclassification (cf. paper Figure 4, Table 2, used for MCS of JSPs later) of agents in varying populations, oftentimes plotting at the same time. Results are saved to /Data. These scripts need you to run `PRELIMS` of the main script (1.). 

	- `Misclassification_P_all.R`: with 100 genuine agents and 100 nefarious agents of each type 	(default settings). Plots, and returns/saves misclassification numbers split by genuine and *each type 	of* nefarious agents, and returns/saves misclassification numbers stored in confusion matrix (only split by nefarious and genuine type, not by all nefarious subtypes). If you run this script top to bottom, you get all results for P_all in 500 voting rounds. 

	- `Misclassification_P_all_robustness.R`: like the just above script, meant to play around with for robustness checks. Robustness checks, varying voting rounds. Returns misclassification numbers split by genuine and *each type of* nefarious agents. Plots, and returns/saves misclassification numbers split by genuine and *each type of* nefarious agents, and returns/saves misclassification numbers stored in confusion matrix (only split by nefarious and genuine type, not by all nefarious subtypes). Manually adapt number of voting rounds to run script for in top of script. 

	- `Misclassification_small_populations_P_A_up_P_D_up_L_up.R`: populations P_A_up, P_D_up, P_L_up. Saves misclassification numbers for smaller subpopulations. 


	- `Misclassification_large_population_robustness.R`: Robustness checks: this file reads classification results from populations with #### 1000 #### genuine agents in it (large population) and plots mean misclassifications and saves results as confusion matrices. Manually adapt number of voting rounds at top of script. 



	

3. Script to produce scatterplot (cf. paper Figure 3)

	- `Scatterplot_U_q_D_q.R`
