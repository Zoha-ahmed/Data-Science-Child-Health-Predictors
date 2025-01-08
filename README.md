# Data-Science-Child-Health-Predictors
This project investigates the distribution and relationships among child health intervention coverages and care-seeking behaviors using an international dataset. The goal is to identify factors contributing to low intervention coverage and healthcare gaps to inform global health policies and resource allocation.

Child health serves as a critical indicator of a nation's development and healthcare accessibility. By exploring vaccination rates, care-seeking behaviors, and preventive interventions, this analysis aims to provide actionable insights to reduce child mortality and improve healthcare outcomes globally.

Dataset Description
Source: UNICEF’s public data repository.

Content: Vaccination coverage rates, care-seeking behaviors, and preventive health interventions across multiple countries.

Handling Missing Data: Missing values are marked as NA to ensure transparency and reliable statistical analysis.

Key Analyses
1. Exploratory Data Analysis (EDA)
Visualizations: Histograms, box plots, and correlation plots to explore distributions and relationships.
Findings:
High variability in vaccination coverage rates (e.g., BCG, DTP1, Polio3).
Gaps in care-seeking behaviors for ARI and fever treatment.
2. Statistical Modeling
Linear Regression:
Target: Predict BCG vaccination coverage.
Key Predictors: DTP1, Polio3.
Results: R² = 0.78, Accuracy = 82%.
Random Forest Regression:
Highlights non-linear interactions.
Feature Importance: DTP1, MCV1, Polio3.
K-means Clustering:
Identified 3 country clusters based on intervention metrics.
Actionable insights for targeted policy recommendations.

Results and Insights:
Strong correlation between vaccination rates indicates robust immunization programs improve multiple interventions.
Clustering reveals significant regional disparities, highlighting areas for prioritized resource allocation.
Predictive models demonstrate the importance of integrated healthcare approaches.
