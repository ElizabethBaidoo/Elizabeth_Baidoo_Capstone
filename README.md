# Antimicrobial susceptibility patterns of Gram-negative uropathogens : 
# Exploratory data analysis of a South West Nigeria tertiary hospital dataset,examining resistance patterns across organisms and antibiotics

## Dataset
Source: Otaigbe, Idemudia; ELIKWU, CHARLES; Ebeigbe, Ejime (2023), “Dataset on Antibiotic susceptibility patterns of Gram negative uropathogens from patients in a tertiary hospital in South west Nigeria”, Mendeley Data, V1, 
 Description: This dataset contains 3613 rows and 29 columns. Each row represents an antibiotic susceptibility test result for a bacterial isolate obtained from a patient sample. Key variables include sex, ward/clinic, specimen, culture, organisms, antibiotics tested, antimicrobial susceptibility results

## Objectives 
1. To identify the most frequently isolated Gram-negative uropathogens
2. To determine the antimicrobial susceptibility profiles of the isolates
3. To determine the prevalence of multi-drug resistance among gram-negative isolates
4. To identify antimicrobial agents with comparatively high and low resistance rates among the gram-negative isolates

## Methodology
Workflow:
Tools:tidyverse, janitor, readxl, skimr, stringr, scales, ggplot2

## Results and Data Presentation
Refer to report (Project_Report_Elizabeth_Baidoo_capstone)

## Repository structure
├── data/
│   ├── raw/            # Original dataset (as sourced from Mendeley Data)
│   └── processed/      # Cleaned dataset used for analysis
├── scripts/             # R scripts for cleaning, transformation, and analysis
├── outputs/
│   ├── figures/         # Charts and visualizations (refer to report)
│   └── tables/          # Summary tables (refer to report
├── report/               # Full project report
└── README.md

## How to Reproduce
Clone this repository.
Open the R project/scripts in RStudio (or your preferred R environment).
Run the cleaning script(s) to reproduce the tidied dataset from the raw data.
Run the analysis script(s) to reproduce the summary statistics and visualizations

## Citation
1. Data source: Otaigbe, Idemudia; ELIKWU, CHARLES; Ebeigbe, Ejime (2023), “Dataset on Antibiotic susceptibility patterns of Gram negative uropathogens from patients in a tertiary hospital in South west Nigeria”, Mendeley Data, V1,
2. Journal article: Otaigbe, I. I., Ebeigbe, E., Okunbor, H. N., Oluwole, T. O., & Elikwu, C. J. (2023). Antibiotic susceptibility profiles of Gram-negative bacterial uropathogens in a tertiary hospital, southwest Nigeria. African Journal of Clinical and Experimental Microbiology, 24(3), 299–304. https://doi.org/10.4314/ajcem.v24i3.10
