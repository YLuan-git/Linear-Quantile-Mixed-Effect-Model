# Linear-Quantile-Mixed-Effect-Model
In this page, we included our project report in pdf file and six Jupyter notebook files in R environment:
1.	Practice.ipynb
In this file, we practiced using the orthodontic data provided by the lqmm package itself to familiarize with the settings of this R package.

2.	Intercept Raw Data Set.ipynb
In this file, we input our raw data and check how the data looks like. Then we found that the raw data contains too much noise and misses some data. So we choose to clean up the data by computing mean energy expenditure in the first place.

3.	Interpret data set after averaging.ipynb
In this file, we input the data file after formatting and calculating mean energy expenditure data. Also, we plot some graphs to visualize the data set in the first place to choose the parameters to use in our model.

4.	lqmm using normal mode but different covariance calc.ipynb
In this file, we attempt to apply lqmm model to our data set with normal mode. We tried different covariance calculation methods. We found that the normal mode has many non-convergence problem. Also, the covariance calculation has limited effect on the results.

5.	lqmm using robust mode and different control setting.ipynb
In this file, we shift to robust mode and significantly reduce the warning messages regarding the non-convergence problem. We also try to use two different optimization methods (gradient search and derivation free). The result shows that traditional gradient search method is appropriate for our project. We also attempt different control settings including different max iteration and error limit.

6.	Plot.ipynb
In this file, we plot the result of one set of the good results of quantile regression as well as linear mean regression.
