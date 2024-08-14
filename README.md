# README

## **Description** 

This repository contains all the code and data needed to replicate the optimisation experiments conducted in our study, as well as the results we generated.
Please download entire repository and open NSGA2_bees.Rproj in order to run the code.

### **code/** 
Includes all new and modified functions required to run the model in R (NSGA2R package installation also required (Tsou, 2022)). 

### **data/**
Includes all model data required to run the model in R, as well as to reproduce all analyses and plots included in the manuscript.

### **results/** 

#### **optimisations/** 
Includes individual RDS files containing the 'raw' results of each separate numerical trial.

#### **results RDS file **
Compiles the results of the above RDS files (i.e. each repeat of each experiment), including the assignment of land covers to final landscape populations by field ('populations') or pixels ('pixels'), the fitness score of each optimised landscape ('fitnessScores') and the patch density of each optimised landscape ('patchDensity).

#### **CSV files:**

| Name | Description |
| --- | --- |
|**fitnessScores.csv**| a CSV file containing the mean fitness score of each generation of landscapes for each objective in each optimisation experiment.|
|**landcoverMeasures.csv**| a CSV file containing various landscape/ landcover metrics of each landcover type in each initial and final landscape in each optimisation.|
|**objective_scores.csv** / **objective_scores_optimised_region.csv**| CSV files scoring each solution landscape for each objective/landscape 'end-user' using the relevant algorithm fitness function. First across the whole landscape and second only in the optimised region.|
|**patchDensity.csv**| a CSV file containing landscape metrics of each solution landscape, as determined using the "landscapemetrics" package in R (Hesselbarth et al., 2019).|
|**populations.csv**| a CSV containing the land cover assigned to each field of each solution landscape in the final generation of each optimisation.|

