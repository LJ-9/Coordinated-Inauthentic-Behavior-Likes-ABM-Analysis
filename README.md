# Coordinated-Inauthentic-Behavior-in-Likes-Agent-based-Model-Analysis
This repository contains code to reproduce the results in the paper Detecting Coordinated Inauthentic Behavior in Likes on Social Media: Proof of Concept by Laura Jahn and Rasmus K. Rendsvig

Please cite the paper when using the code:  

Jahn, Laura and Rendsvig, Rasmus K., "Detecting Coordinated Inauthentic Behavior in Likes on Social Media: Proof of Concept", 2022.

```
  @misc{JahnRendsvig22Coordination,  
    author = {{Jahn, Laura and Rendsvig, Rasmus~K.}},
    title = {{Detecting Coordinated Inauthentic Behavior in Likes on Social Media: Proof of Concept}},  
    year = {2022},   
  }
 ```
This repository contains three folders:

- /ABM_Classification: contains all scripts to run the ABM (generate vote data) and execute classification analysis on varying populations, as well as scripts that produce misclassification numbers and plots

- /Data: here is where all data is stored that is produced in /ABM_Classification

- /MCS: scripts to calculate majority correctness scores of Jury Selection Procedures

- please see further READMEs throughout folder hierarchy for guidance.

 
** Change in variable naming **  
You may encounter that we refer to our agents as genuine and nefarious in the code and further READMEs deeper in the folder hierarchy. Please note that in the process of this research project, we changed the name of *genuine* agents to *authentic* agents in the paper. This also affects a change in notation from G to A. Similarly, we changed the name of *nefarious* agents to *inauthentic* agents. Among the inauthentic agent types, formerly nefarious agent types, we renamed the Amplifier agents (A) to Booster agents (B). These changes do not have any consequences in the technical modelling steps.****
