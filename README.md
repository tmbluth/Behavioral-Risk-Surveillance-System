# Behavioral-Risk-Surveillance-System

Background:
This project will explore the behavioral factors that are linked to chronic disease. The CDC has been collecting behavioral, demographic, and disease info on thousands of Americans that can be used in a mind-boggling number of applications. I hope to explore some of the linkages to chronic disease. Once I understand the data better my goal is to make a well-informed model that will be able to calculate the risk of certain chronic diseases that others can use to estimate their risk for such diseases. This risk score will likely be a class probability (likely to have / get disease vs not) but if a regression approach is more suitable due to outcome variable distribution then a continuous outcome will be predicted. 

Methods:
1.	Feature selection: there are hundreds of columns so narrowing it down will be important
2.	Feature interpretability – LIME (Local Interpretable Model-agnostic Explanations) is an R package that helps the analyst uncover the features of importance and their magnitude in the decision-making process on predictions
3.	SVM – this is a powerful algorithm that I would love to learn more about and is apt to crunch thousands of variables. If this algorithm is not fit for the task I will likely default to a decision tree based approach (random forest for classification, boosted tree for regression)
4.	Parallel computing – R’s `doParallel` and `parallel` packages will be helpful to use to speed up computation time. 
5.	Distributed computing – using Spark to speed up computation time and prevent big data difficulties 
These data sets are in csv format on Kaggle and span from 2001 to 2015. Due to this time component, descriptive time series methods will be used. Collectively the data will hold about 6 GB of info, so I hope to use Spark during the heavier computations related to data manipulation and modeling. Though it is powerful, I only have one computer and will likely need multiple to make a small cluster and speed up computation time.

Potential Obstacles:
Since this data is collected from phone surveys I expect there to be plenty of missing data. Also, the fact that this is a social science / health study there will likely be high variability. Another setback may come from using Spark. Since `sparklyr` is not supported as readily as other packages it will be more challenging to work around. Many of the useful R functions normally used in an R analysis may not be available while using Spark. 

Project Benefit:
Such an analysis will: 
1.	Allow better understanding of public health risks
2.	Understand representative samples of the American population’s health across time 
3.	Will produce a tool to calculate personal risk
The risk calculator tool will give risk of different chronic conditions and will be more comprehensive than other population specific calculators found on the web. It can be used by anyone and improved by anyone with similar interests. Just start with a pull request!
