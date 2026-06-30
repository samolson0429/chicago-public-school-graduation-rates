# Chicago Public School Graduation Rates

## Using machine learning techniques to analyze factors that influence Chicago Public School graduation rates

<img width="1536" height="1024" alt="image" src="https://github.com/user-attachments/assets/56747e77-7ae7-43fd-a8c9-7194e4b31848" />

## Overview

Improving high school graduation rates is an important objective for school districts, policymakers, and communities. While many factors may contribute to student success, identifying the variables that are consistently associated with graduation rates can help educators prioritize interventions and allocate resources effectively.

Using publicly available Chicago Public School data, this project compares several analytical approaches to determine which variables are most strongly associated with graduation rates.

## Research Question

Which school and student characteristics are most strongly associated with high school graduation rates in Chicago Public Schools, and how consistently are these variables selected across different analytical models?

## Tools Used

- R
- Python
- ggplot2
- ggpubr
- pandas
- numpy

## Analytical Techniques

- Data cleaning
- Exploratory data analysis
- Feature engineering
- Stepwise regression
- LASSO regression
- Logistic regression
- Random forest
- Feature importance
- Model comparison

## Key Findings

Several variables were consistently identified across multiple analytical approaches, suggesting they have strong relationships with graduation rates.

The most consistently selected variables included:

- Student mobility rate
- Culture climate rating
- Freshmen on track
- Involved families
- Student attendance
- Percentage of students receiving special education services

Student mobility rate was selected across all four models, while the remaining five variables were selected in three of the four models.

## My Contributions

This project was completed as part of a team in Georgia Tech's M.S. in Analytics program.

My primary contributions included:

- Developing the stepwise regression model in R
- Assisting with data integration & cleaning
- Contributing to model evaluation & interpretation
- Assisting with report development & documentation

## Repository Contents

- graduation_analysis_report.pdf: Detailed project report
- stepwise_regression.R: R implementation of the stepwise regression analysis
- data folder: Public datasets used for the analysis

## Dataset

This project used publicly available datasets from data.gov.

- The "progress_report" data files include information about school performance
- The "school_profile" data files include demographic and high-level school information, including the response variable of graduation rate
- The "combined.txt" file represents the combines raw data files and was used to perform the stepwise regression analysis in R
