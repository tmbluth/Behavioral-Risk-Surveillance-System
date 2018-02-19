# Behavioral-Risk-Surveillance-System

----

Background:

This project will explore the behavioral factors that are linked to chronic disease. The CDC has been collecting behavioral, demographic, and disease info on millions of Americans that can be used in a large number of applications. 
I hope to explore some of the linkages to chronic disease. My goal is to make a well-informed model using an intersection of health behavior theory and feature selection that will be able to calculate the risk of certain chronic diseases. Anyone willing to fill out a moderately long survey can use this tool to estimate their risk for such diseases. 

----

Methods:

1.	Feature selection: there are hundreds of columns so I have narrowed things down to only relevant questions to fit into a survey. Theory and model based methods were used.
2.	Feature interpretability – LIME (Local Interpretable Model-agnostic Explanations) is an R package that helps the analyst uncover the features of importance and their magnitude in the decision-making process on predictions
3.	SVM or Naive Bayes – these are a powerful algorithms with the ability to crunch thousands of variables in a reasonable amount of time. If either algorithm is not fit for the task I will likely default to a decision tree based approach
4.	Computational Boosts – R’s `doParallel` and `parallel` packages will be helpful to use to speed up computation time. If possible, accessing the computer's GPU through `gputools` or `gpuR` will speed up the process even further.

These data sets are in csv format on Kaggle and span from 2001 to 2015. Only years 2011 to 2015 will be used however, due to difference in survey structure starting in 2011. Data can be found here: https://www.kaggle.com/cdc/behavioral-risk-factor-surveillance-system/data
Prevalence statistics across time are already available on the CDC's website (https://www.cdc.gov/brfss/brfssprevalence/index.html) and will not be persued here. 

----

Potential Obstacles:

Since this data is collected from phone surveys I expect there to be plenty of missing data. Also, the fact that this is a social science / health study there will likely be high variability. 
It will also be a bit of a challenge to clean responses values since many response types will be numeric though they should not be. Continuous, categorical, and ordinal data will need to be made from the numeric representations and non-numeric data must be dummy coded.

----

Project Benefit:

Such an analysis will...
1.	Allow better understanding of public health risks
2.	Will produce a tool to calculate personal risk of chronic diseases 
3.	Will be open for public use

The risk calculator tool will give risk of different chronic conditions and will be more comprehensive than other population specific calculators found on the web. It can be used by anyone and improved by anyone with similar interests. Just start with a pull request!
