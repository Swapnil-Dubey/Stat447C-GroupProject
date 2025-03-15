# Stat447C-GroupProject

## Team information
Team: Swapnil Dubey: 70081476, Lawrence Ma: 41896937\
Theme: Bayesian vs frequentist Survival Analysis



## Candidate Datasets

1. Global Water Consumption Dataset()
Source: Kaggle - https://www.kaggle.com/datasets/atharvasoundankar/global-water-consumption-dataset-2000-2024
Structure:
Time-to-event: Years until country reaches "High" water scarcity
Event: Water Scarcity Level = High
Covariates: Agricultural water use (%), groundwater depletion rate (%), rainfall
(mm)
Censoring: 34% of countries right censored (not reaching "High" by 2024)
Novelty: First survival analysis of water scarcity transitions, prior work uses
regression
2. COVID-19 Variants Survival Data
Source: Kaggle https://www.kaggle.com/datasets/lumierebatalong/covid-19-variants-survival-data/data
Structure:
Time-to-event: Epidemic duration (days)
Event: End of COVID wave
Covariates: Variant type, mortality rate, growth rate
Censoring: 69% right censored epidemics
Novelty: Extends existing Cox PH studies with Bayesian survival analysis.



## Potential Approaches

Global Water Consumption Dataset
Research Question: How do agricultural water use and rainfall affect the time-to-transition into
high water scarcity among countries?
Bayesian Approach:
- Fit a hierarchical Weibull survival model incorporating region-specific random effects
(e.g., contrasting the Middle East with Europe).
Use informative priors for agricultural water use
Compute posterior probabilities, such as check if P(agricultural use increases risk) is
exceeding 90%.
Frequentist Baseline:
- Cox Proportional Hazards model with time-varying agricultural use.
- Compare hazard ratios (HR) and p-values against Bayesian posteriors.

COVID-19 Variants Dataset
Research Question: Do SARS-CoV-2 variants differ in their impact on epidemic duration?
Bayesian Approach:
- Competing risk survival model with variant-specific priors
- Estimate variant risk probabilities (e.g., P(Delta shortens duration) vs Omicron).
Frequentist Baseline:
- Stratified Cox PH by variant type.
- Compare frequentist hazard ratio to Bayesian posterior risk distributions.



## Team Member Contribution Plan (Task division)
1. Version control: Both team members will frequently update their changes and progress
to their specific GitHub branches of the project.
2. Task Division Plan:
  a. Project Management & Coordination
      Member A: Manage meetings, set deadlines, and organize the Git repository.
      Member B: Track progress using GitHub Issues and ensure timelines are met.
  b. Literature Review & Proposal
      Member A: Conduct literature review on Bayesian vs. frequentist methods.
      Member B: Refactor and extend the proposal, describe datasets, and outline
      potential approaches.
  c. Data Collection & Preprocessing
      Member A: Collect data and perform EDA.
      Member B: Conduct data preprocessing and ensure data is clean and organized.
  d. Model Development & Implementation
      Member A: Implement Bayesian model and conduct posterior analysis.
      Member B: Implement frequentist model and evaluate performance.
      Both: Compare models and create visualizations.
  e. Report Writing
      Member A: Write sections on problem formulation, Bayesian model, and theory.
      Member B: Write data analysis, frequentist model, and evaluation.
      Both: Combine and refine the final report.
  f. Version Control
      Both: Commit regularly to Git with clear messages and document individual
      contributions.
3. Accountability:
  a. Hold frequent meetings to gauge progress of each member over Discord.
  b. Frequently check Discord or reply promptly to keep the conversation going.
  c. Try to stay on top of deadlines to be able to complete this project in time.
