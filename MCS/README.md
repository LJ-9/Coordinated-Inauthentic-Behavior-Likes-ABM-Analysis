### This folder contains the files for calculating the majortiy correctness scores (MCSs) of juries.

While coding, we named many things "CW", short for "Correct Wins", i.e., that the majority vote was correct.
For fear of typos, this has not been changed in subsequent files.

Main Files:

- `CW1 Mean-SD Tables.R`:
	contains functions that define MCS and for generating Mean and SD tables of such.

- `CW2 Table Generation.R`:
	generates the tables with MCSs, prior to jury trimming by MCSs. This may take a long time.

- `CW3 Table Names.R`: 
	names the generated MCS tables, for plotting.

- `CW4 Plot A.R` and `CW4 Plot B.R`: 
	generate TikZ files with plots of MCSs -- these are the paper's Figures 1 and 2.

- `JSP Effect_Group_2.R`, `JSP Effect_Group_6.R`, `JSP Effect_Group_9.R` and `JSP Effect_All-Groups.R`: 
	produces the MCSs of the juries that are obtained through our Jury Selection Procedures. 
	The Effect files consult agent misclassification files generated by
		- `/ABM_Classification/Misclassification_large_population_robust.R`,(this populations is discarded in JSPs - things only improve here (larger n and r), analysis focused on the more blurred and difficult to manoeuvre populations, below two files:)
	- `/ABM_Classification/Misclassification_P_all.R`
	- `/ABM_Classification/Misclassification_small_populations_P_A_up_P_D_up_P_L_up.R`

All the Effect files conclude with producing a plot showing the main results. 
	This plot must be saved manually!
	Running these files may take a long time!

x files:
	the x files contains various minor functions and color definitions shared among the ramining files.
