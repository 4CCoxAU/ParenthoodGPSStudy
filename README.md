# Depression is Associated with Spatial Restriction in Fathers and Mothers on Parental Leave: a GPS Tracking Study

This repository contains data, code and supplementary materials for the following paper:

Depression is Associated with Spatial Restriction in Fathers and Mothers on Parental Leave: a GPS Tracking Study.

## Overview

This project uses continuous GPS tracking of 171 first-time Danish parents to examine how spatial mobility, mental health, and sleep quality change during parental leave periods. The study reveals that depression symptoms create detectable spatial signatures even within the naturally constrained context of parental leave.

**OSF link:** https://osf.io/d4mt6/?view_only=15ee29712b394f0eac18a5507cc20639

## Repository Structure

```
analysis_code/
├── 00_MasterScript.Rmd           # Main analysis orchestrator
├── 01a_LeaveDifferences.Rmd      # Mobility differences analysis (RQ1)
├── 01b_LeaveDifferencesFigures.Rmd # RQ1 visualization code
├── 02a_DepressionSleepAnalysis.Rmd # Mental health analysis (RQ2)
├── 02b_DepressionSleepFigures.Rmd # RQ2 visualization code
├── 03a_DepressionAnalysis.Rmd    # Depression-mobility associations (RQ3)
├── 03b_DepressionAnalysisFigures.Rmd # RQ3 visualization code
├── 04_SupplementaryFigures.Rmd   # Additional visualizations
├── 05_ControlModels.Rmd          # Robustness checks and control analyses
├── 07_DeletedScenes.Rmd          # Archived analysis code
├── functions.R                   # Custom analysis functions
├── readindata.R                  # Data preprocessing and loading
├── models/                       # Saved Bayesian model objects
├── QuickPlots/                   # Generated figures and plots
└── StatsForBodyText/             # Statistical summaries for manuscript
```

## Research Questions

- **RQ1:** Do mothers and fathers exhibit distinct spatial mobility patterns during parental leave?
- **RQ2:** How do mental health and sleep quality change during leave transitions?
- **RQ3:** Do depression symptoms predict spatial mobility beyond caregiving constraints?

## Key Features

- **GPS Processing Pipeline:** DBSCAN clustering for stay detection and mobility metric calculation
- **Bayesian Multilevel Models:** Distribution-specific modeling (hurdle, zero-inflated, ordinal)
- **Mental Health Integration:** Edinburgh Postnatal Depression Scale and daily sleep quality
- **Gender-Comparative Analysis:** Separate examination of maternal vs. paternal mobility patterns

## GPS-Derived Mobility Metrics

**Mobility Range:** Maximum distance from home, radius of gyration  
**Out-of-Home Activity:** Time spent away from home across morning/afternoon/evening periods  
**Spatial Complexity:** Location variance, spatial entropy, activity space area, movement directionality

## Analysis Scripts

- **`01a_LeaveDifferences.Rmd`:** Mobility patterns across leave periods (RQ1)
- **`01b_LeaveDifferencesFigures.Rmd`:** RQ1 visualizations
- **`02a_DepressionSleepAnalysis.Rmd`:** Mental health and sleep analysis (RQ2)
- **`02b_DepressionSleepFigures.Rmd`:** RQ2 visualizations
- **`03a_DepressionAnalysis.Rmd`:** Depression-mobility associations (RQ3)
- **`03b_DepressionAnalysisFigures.Rmd`:** RQ3 visualizations
- **`04_SupplementaryFigures.Rmd`:** Study overview and supplementary plots
- **`05_ControlModels.Rmd`:** Robustness checks and sensitivity analyses
- **`functions.R`:** Custom functions for GPS processing and modeling

## Statistical Approach

**Bayesian Framework:** Models implemented using `brms` with Stan backend  
**Distribution-Specific Models:** Hurdle log-normal, zero-one-inflated beta, ordered logit  
**Multilevel Structure:** Participants nested within leave periods with role-specific variance  
**Evidence Ratios:** Bayesian hypothesis testing using posterior probability ratios

## Key Findings

1. Both parents dramatically reduce mobility during leave, but organize movement differently
2. Depression and sleep quality track with active caregiving responsibilities  
3. Higher depression predicts both complete home stays and shorter trips when mobile
4. Gender-specific pathways: fathers show binary withdrawal, mothers show gradual restriction

## Requirements

- R (≥ 4.0), Stan, `brms`, `tidyverse`, `geosphere`, `dbscan`

## Usage

1. Execute analysis scripts in numerical order starting with `01a_LeaveDifferences.Rmd`
2. Generate figures using corresponding `*Figures.Rmd` scripts  
3. Run `05_ControlModels.Rmd` for robustness checks

## Data Availability

Raw GPS data cannot be shared due to privacy concerns. Simulated dataset available to test code (see supplementary materials for more information).