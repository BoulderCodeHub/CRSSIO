description of StVrain_NaturalFlow_Documentation

The code and data in this directory obtains/formats St Vrain natural flow, fits and evaluates several statistical models to predict St Vrain natural flow from other flow points in CRSS, and reports the parameters of the model chosen for implementation in the CRSSIO package.

######### DATA ACQUISITION AND FORMATTING SCRIPTS #####

historical daily cfs to annual acre feet.R: takes daily cfs gage data from from https://dwr.state.co.us/Tools/Stations/SVCLYOCO?params=DISCHRG and converts to annual (CY) acre feet. Saves data to 'Historical CY Flow - St Vrain Lyons.csv'

Get St Vrain Natural Flow.R: formats St. Vrain natural flow data from two sources into one csv file. The two sources are StateMod (file SP2016_BCx.xbm) and a regression from the state of CO that estimates natural flow given cy acre feet of gage flow (using the csv file produced from previous script)

######### Model testing scripts ############################

relationship exploration.R: calculates correlation natural flow of 29 sites in CRSS to St Vrain natural flow over the historical period. Tests both intervening and total natural flow. Conclusions: GlenwoodSprings highly correlated with St Vrain, both total and intervening natural flow (both corr of 0.77) . There is no meaningful correlation between any gage and the residuals of a St Vrain ~ Glenwood linear model.

Linear log linear and quantile regression.R : tests simple linear, log-log linear, and quantile regression using GlenwoodSprings total NF as predictor. All perform similarly, saw no reason not to use linear.

PCA Regression.R: perform principal component analysis on total natural flow at all 29 sites to create uncorrelated, synthetic predictor variables (principal components) to use in regression. Worse performance than simple linear.

kNN v Linear.R : compares inverse-distance-weighted KNN regression to simple linear, LOESS, and smoothed spline. Performs 100 iterations of 10-fold cross validation and reports RMSE and R2 for each. Linear performs best.


############ Chosen model ###################################

report linear model coefficients.R: Reports the slope and intercept of the fitted linear model. These parameters are used in the CRSSIO function 'st_vrain_nf_calc' to estimate. 

A summary of the model and it's performance on training data is below.

Call:
lm(formula = StVrain ~ GlenwoodSprings, data = df_complete)

Residuals:
   Min     1Q Median     3Q    Max 
-59463 -16207  -3573  13519  62745 

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)     2.045e+04  8.030e+03   2.547   0.0122 *  
GlenwoodSprings 4.671e-02  3.657e-03  12.772   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 22050 on 112 degrees of freedom
Multiple R-squared:  0.5929,	Adjusted R-squared:  0.5893 
F-statistic: 163.1 on 1 and 112 DF,  p-value: < 2.2e-16

> 
> 
> print(md$coefficients)
    (Intercept) GlenwoodSprings 
   2.045355e+04    4.670764e-02 