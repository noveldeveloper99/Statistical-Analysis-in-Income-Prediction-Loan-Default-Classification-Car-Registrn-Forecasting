# Statistics-for-Data-Analytics
Module for Statistical Analysis in R Studio

This repository contains multiple projects including Income Prediction using Regression (1), Time Series Forecasting for cars registration numbers (2) and Binary Classification using Logistic Regression (3).

### Datasets
Attached in the repository as follows:

IncomeData.csv

CarRegistrations.csv

Default.csv

### Technologies

R Studio and SPSS.

## Income Prediction with Regression

This project (1) determines the Income of a particular individual using a Multiple Linear Regression Model employing suitable feature selection and Regression diagnostics on a raw dataset that provides the financial profile for over four thousand individuals.
![image](https://user-images.githubusercontent.com/98535942/217416019-e7744ba0-f218-41a8-930b-a2490abe3c9a.png)

Our final model from Iteration 5, exhibits high accuracy of 0.2 residual errors and utilizes almost all of the reliable predictors (Adj. R2 score of 92.8%) for Income prediction from the given dataset satisfying all of the assumptions of the regression technique to become a best linear unbiased estimator model. Further analysis and model optimization can be done using cross-validation techniques which will explored in detail at a later stage.

## Time Series Forecasting and Binary Classification with Logistic Regression

This project (2,3) aims to demonstrate the application of two of the most widely used statistical techniques – Time Series Analysis and Logistic Regression. Time Series Analysis allows to grasp the effects of trend, seasonality and noise on the concerned response variable and build an accurate time-dependent forecast. On the other hand, Logistic Regression is typically used to classify a dichotomous response variable regressed upon a combination of categorical and/or numerical predictors. Both the use-cases are diagnosed and optimized for performance and fit using expansive descriptive statistics, thorough model diagnostics and relevant performance metrics.

### A. Time Series Analysis Use case
The final model for SARIMA turned out to be model #4 with (p,d,q)(P,D,Q) values (3,1,4), (1,2,3) and AIC: 5581.62 and RMSE: 2194.99. This model had improved model accuracy and fit and satisfied all assumptions.

![image](https://user-images.githubusercontent.com/98535942/217415881-8539b230-ce74-4e24-af5f-39b7588bc774.png)

### B. Logistic Regression Use case
We observed several models and found that our final logistic regression model presented an Accuracy of 77.91 %, Sensitivity of 73.1% and Specificity 81.8% i.e. the model can correctly classify 77.9% of both the classes while it can classify the “default: 1” category with 73.1% accuracy and “default: 0” category with 81.8% accuracy.

![image](https://user-images.githubusercontent.com/98535942/217415824-89276e83-9ae1-40d6-a07b-9d21d5f0e3f8.png)

The evaluation of final model’s Odds ratios (Fig.15) informs that for every 1000 units increase (thousands) in creditdebt the odds of getting a default :1 increases by 1.6 times while every unit increase (years) in othdebt increases the odds of getting a default: 1.
Finally, the ROC plots and Jitter-plots in Fig. 16 and 17 provides us with the relationship between Sensitivity and Specificity, and prediction probabilities for classification of binary classes respectively.
