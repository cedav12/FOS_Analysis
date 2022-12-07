# FoS course Regression Analysis
We have cross-sectional data about Fundamentals of Statistics (FoS course) 2018 collected among students in Uzbekistan. 
Data are stored as [`dataForStudents.xlsx`](data\dataForStudents.xlsx) file. The legend related to this data could be found in file [`Codebook.pdf`](data\Codebook.pdf).
The goal of this analysis is to inspect, whether there is a relationship between number of attended seminars and final grades.

The code is written in [`R`](IV_analysis.R) and knitted into pdf with [`RMarkdown file`](Fos.Rmd). The output of the analysis can be found in [`Fos.pdf`](Fos.pdf).

Overall we have estimated several models,since we suspected endogeneity of explanatorxy variable nlesson, we sticked to IV model and Simple Regression model. 
To choose the most appropriate model, we tested several assumptions and after performing `Hausman.systemfit` test we could not reject, that both models are consistent.
This implies, that after all we chose Simple Regression model, which should be assymptotically better.

We cannot reject OLS assumptions 1-6, thus we assume the model is BLUE(best linear unbiased estimator). 
We also identified, the positive relationship betwwen number of seminars attended and the final grade.
