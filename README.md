# Risk_Prediction_Model-R_Script
The repository contains R scripts we used for the master thesis "Risk Prediction Model: Assessing the evolution of risk factors to use in the calculation of new dynamic risk scores".

# Data extractions and preprocessing
"Data_extraction.R" file contains all the extractions we have done from the IMA database. Make sure to run this file first.

# Cohort analysis
"Cohort_analysis.R" file contains a step-by-step analysis of the target cohort and this file should be run after the "Data_extraction.R".

# Exploratory analysis
"Exploratory_analysis.R" contains all the visualizations and exploration. Make sure to run this file after "Cohort_analysis.R".

# Imputation
The missing data exploration, assumption testing, imputation, and analysis are performed in the file " Imputation_30%.R". Make sure to run this file after "Cohort_analysis.R"

# Models and Predictions
Please make sure to run "Data Processing for Analysis.R" script before running "Survival Analysis.R", "Final JM Men.R" and "Final JM Women.R"
