# CapstoneProject
Vivek Appadurai  
February 20, 2016  

## Loading Libraries


```r
library(dplyr)
library(ggplot2)
library(knitr)
library(gridExtra)
library(pROC)
library(leaps)
library(MASS)
library(e1071)
library(mlbench)
library(caret)
library(randomForest)
library(rpart)
library(ROCR)
```

## Reading Data


```r
variableInfo <- read.table("Names.txt", 
                           header = T, 
                           sep = "\t", 
                           fill = NA, 
                           quote = "",
                           stringsAsFactors = FALSE)

ticDataTraining <- read.table("ticdata2000.txt", 
                              header = F, 
                              sep = "\t", 
                              fill = NA, 
                              quote = "", 
                              stringsAsFactors = FALSE)

ticDataTest <- read.table("ticeval2000.txt", 
                          header = F, 
                          sep = "\t", 
                          fill = NA, 
                          quote = "",
                          stringsAsFactors = FALSE)

targets <- read.table("tictgts2000.txt",
                              header = F,
                              stringsAsFactors = FALSE)
```

## Data Cleaning


```r
colNames <- as.vector(variableInfo$Name)
names(ticDataTraining) <- colNames
ticDataTest <- cbind(ticDataTest, targets)
names(ticDataTest) <- colNames

ticDataTraining <- ticDataTraining %>% mutate(totalCaravanPolicies = sum(CARAVAN))
```

## Exploratory Data Analysis

Since the number of caravan policy holders within the training data is very low (348/5822 or apprxomiately 6%), 
its best to explore the assocaitions within the data by looking at the fraction of policy holders at each value 
for the varaibles along with the proportion of population at that particular value



#### Plotting Using Grid Arrange


```r
grid.arrange(plotHomesOwned, plotHouseHoldSize, PlotAvgAge, plotRomanCatholics, plotProtestants, plotOtherReligion,
             plotNoReligion, plotMarried, plotLivingTogether, plotOtherRelation, plotSingles, plotHouseholdNoChild,
             plotHouseholdWithChild, plotHouseholdHighEd, plotHouseholdMidEd, plotHouseholdLowEd, plotHighStatus,
             plotEntrepreneur, plotFarmer, plotMidManagement, plotSkilledLabor, plotUnskilledLabor, plotSocialA,
             plotSocialB1, plotSocialB2, plotSocialC, plotSocialD, plotRentHouse, plotOwnHome, plotOneCar, 
             plotTwoCars, plotNoCar, plotNHS, plotPHS, plotIncome30k, plotIncome30_45k, plotIncome45_75k, 
             plotIncome75k_122k, plotIncome123k, plotAvgIncome, plotPurchasingPower, plotNumThirdPartyIns,
             plotNumThirdPartyInsCont, plotNumThirdPartyInsFirm, plotNumThirdPartyInsFirmCont, plotNumThirdPartyInsAgri,
             plotNumThirdPartyInsAgriCont, plotCarPolicy, plotCarPolicyCont, plotVanPolicy, plotVanPolicyCont,
             plotScooterPolicy, plotScooterPolicyCont, plotLorryPolicy, plotLorryPolicyCont, plotTrailerPolicy,
             plotTrailerPolicyCont, plotTractorPolicy, plotTractorPolicyCont, plotAgriMachinePolicy,
             plotAgriMachinePolicyCont, plotMopedPolicy, plotMopedPolicyCont, plotLifePolicy, plotLifePolicyCont,
             plotPvtAccidentPolicy, plotPvtAccidentPolicyCont, plotFamAccidentPolicy, plotFamAccidentPolicyCont,
             plotDisabilityPolicy, plotDisabilityPolicyCont, plotFirePolicy, plotFirePolicyCont, plotSurfPolicy,
             plotSurfPolicyCont, plotBoatPolicy, plotBoatPolicyCont, plotBicyclePolicy, plotBicyclePolicyCont,
             plotPropertyPolicy, plotPropertyPolicyCont, plotSocialPolicy, plotSocialPolicyCont, ncol = 2)
```

#### Observations:

From the plots the following conditions imply a higher proportion of caravan policy owners than expected:

1. Zipcodes with a less number of low level educated people
2. Zipcodes with very high status people
3. Zipcodes with NO FARMERS
4. Zipcodes with low number of laborers (skilled & unskilled)
5. Zipcodes with low number of people belonging to social classes C & D
6. Zipcodes having a very high number of Home Owners
7. Zipcodes with low or no rented homes
8. Zipcodes having a high car ownership
9. Zipcodes having little to no population with income < 30k
10. Zipcodes with average income households
11. Zipcodes with high purchasing power >= 6
12. Individuals that have a private third party insurance
13. Individuals with high contributions to third party insurance
14. Individuals with one or more car policy
15. Individuals with high contributions to their car policies
16. Individuals with Fire Insurance Policy
17. Individuals making high contributions to fire insurance policies

The above factors are worth further investigation to draw more concrete trends


```r
ggplot(ticDataTraining, aes(x = APERSAUT, y = PPERSAUT, color = as.factor(CARAVAN))) +
    geom_jitter() + 
    xlab("Num. Car Policies") +
    ylab("Contributions") +
    theme_bw() +
    scale_x_continuous(breaks =seq(0, max(ticDataTraining$APERSAUT), 1))
```

![](CapstoneProject_files/figure-html/unnamed-chunk-6-1.png)

The plot indicates that people who have a car policy tend to contribute significantly towards it.


```r
ggplot(ticDataTraining, aes(x = AWAPART, y = PWAPART, color = as.factor(CARAVAN))) +
    geom_jitter() + 
    xlab("Private Third Party Insurance") +
    ylab("Contributions") +
    theme_bw() +
    scale_x_continuous(breaks =seq(0, max(ticDataTraining$AWAPART), 1))
```

![](CapstoneProject_files/figure-html/unnamed-chunk-7-1.png)


```r
ggplot(ticDataTraining, aes(x = ABRAND, y = PBRAND, color = as.factor(CARAVAN))) +
    geom_jitter() + 
    xlab("Num. Fire Policies") +
    ylab("Contributions") +
    theme_bw() +
    scale_x_continuous(breaks =seq(0, max(ticDataTraining$ABRAND), 1))
```

![](CapstoneProject_files/figure-html/unnamed-chunk-8-1.png)

### Automatic Variable Selection, Forward and Backward


```r
ticDataTraining <- ticDataTraining[,1:86]
ticDataTraining$MOSTYPE <- as.factor(ticDataTraining$MOSTYPE)
ticDataTest$MOSTYPE <- as.factor(ticDataTest$MOSTYPE)
ticDataTraining$MOSHOOFD <- as.factor(ticDataTraining$MOSHOOFD)
ticDataTest$MOSHOOFD <- as.factor(ticDataTest$MOSHOOFD)
ticDataTraining$CARAVAN <- as.factor(ticDataTraining$CARAVAN)
ticDataTest$CARAVAN <- as.factor(ticDataTest$CARAVAN)

reg_Backward <- regsubsets(CARAVAN ~., data = ticDataTraining, method = "backward")
```

```
## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax,
## force.in = force.in, : 9 linear dependencies found
```

```
## Reordering variables and trying again:
```

```r
reg_Forward <- regsubsets(CARAVAN ~., data = ticDataTraining, method = "forward")
```

```
## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax,
## force.in = force.in, : 9 linear dependencies found
```

```
## Reordering variables and trying again:
```

### SVM Classifier Using Linear and Radial Kernels


```r
ticData.SVM.Linear <- svm(CARAVAN ~ ., data = ticDataTraining, kernel = "linear", probability = TRUE)
ticData.SVM.Radial <- svm(CARAVAN ~ ., data = ticDataTraining, kernel = "radial", probability = TRUE)
ticData.SVM.Linear.Pred <- predict(ticData.SVM.Linear, newdata = ticDataTest[-86], probability = TRUE)
ticData.SVM.Radial.Pred <- predict(ticData.SVM.Radial, newdata = ticDataTest[-86], probability = TRUE)
```

### Naive Bayes Classifier


```r
ticData.NB <- naiveBayes(data = ticDataTraining, CARAVAN ~ ., laplace = 3)
ticData.NB.pred <- predict(ticData.NB, ticDataTest[-86], type = "raw")
```

### Decision Tree Classifier


```r
ticData.Tree <- rpart(CARAVAN ~ ., data = ticDataTraining, method = "class", 
                      control = rpart.control(minsplit=2, minbucket=1, cp=0.001))
ticData.PrunedTree <- prune(ticData.Tree, 0.0045, control = rpart.control(minsplit=2, minbucket=1))
plot(ticData.PrunedTree, uniform = TRUE, margin = 0.1)
text(ticData.PrunedTree, use.n = T, cex = 0.8)
```

![](CapstoneProject_files/figure-html/unnamed-chunk-12-1.png)

```r
ticDataTraining.PredictTree <- predict(ticData.PrunedTree)
ticDataTest.PredictTree <- predict(ticData.PrunedTree, ticDataTest[-86])
```

### Random Forest Classifier


```r
ticData.RF <- randomForest(CARAVAN ~ ., 
                                   data = ticDataTraining, 
                                   nTree = 100,
                                   importance = TRUE,
                                   proximity = TRUE)

round(importance(ticData.RF),2)
```

```
##              0     1 MeanDecreaseAccuracy MeanDecreaseGini
## MOSTYPE   3.34 -0.79                 3.29            40.83
## MAANTHUI  0.34  1.44                 0.76             2.41
## MGEMOMV   6.80 -2.33                 6.49             5.45
## MGEMLEEF  7.07 -1.09                 6.82             6.72
## MOSHOOFD  8.18 -0.42                 8.48            13.04
## MGODRK    3.22 -0.88                 2.94             5.31
## MGODPR    4.61 -0.33                 4.50            10.31
## MGODOV    5.99 -1.30                 5.66             7.80
## MGODGE    4.41  0.52                 4.68            10.70
## MRELGE    9.22 -2.02                 8.84             8.39
## MRELSA    4.03 -1.45                 3.73             5.23
## MRELOV   11.15 -4.27                10.40             7.43
## MFALLEEN  8.99 -2.74                 8.79             7.88
## MFGEKIND  5.62 -2.89                 4.83            10.20
## MFWEKIND  8.65 -2.23                 8.19            10.63
## MOPLHOOG 10.88 -2.79                10.74             9.72
## MOPLMIDD  8.68 -1.57                 8.56            11.42
## MOPLLAAG 10.34 -1.53                10.59            10.79
## MBERHOOG  6.12 -1.32                 5.99             8.88
## MBERZELF  5.08  0.78                 5.19             5.25
## MBERBOER  6.20 -0.54                 6.16             3.83
## MBERMIDD  6.44  4.88                 8.05            10.83
## MBERARBG  9.28 -2.26                 9.03            10.07
## MBERARBO  7.27 -3.39                 6.62             9.78
## MSKA     10.54 -4.06                10.12             8.98
## MSKB1     6.01 -0.20                 6.13             8.68
## MSKB2     7.48 -1.98                 6.71             9.39
## MSKC      8.52 -1.14                 8.69            10.40
## MSKD      6.84 -0.45                 6.75             5.97
## MHHUUR    8.47 -1.29                 8.53             9.59
## MHKOOP    7.28 -2.11                 7.23             9.34
## MAUT1     9.05 -2.07                 8.68             8.25
## MAUT2     5.56 -3.08                 4.96             7.32
## MAUT0     9.26 -1.23                 9.37             7.21
## MZFONDS   6.88 -2.27                 6.90             8.84
## MZPART    7.30 -5.08                 6.58             8.98
## MINKM30   7.71 -1.38                 7.75             8.85
## MINK3045  5.96 -1.25                 5.69            10.45
## MINK4575  5.95 -0.70                 5.77             9.80
## MINK7512  8.55 -1.76                 8.22             8.13
## MINK123M  2.52  1.03                 2.79             3.26
## MINKGEM   9.09  1.76                 9.71             8.08
## MKOOPKLA  5.39  2.19                 6.14            10.26
## PWAPART   5.25  3.22                 6.33            11.65
## PWABEDR  -1.91 -2.45                -2.49             1.14
## PWALAND   1.72 -0.56                 1.60             0.41
## PPERSAUT -2.04 17.29                 3.20            18.72
## PBESAUT  -1.81  0.43                -1.79             0.81
## PMOTSCO  -1.04 -0.19                -1.08             3.48
## PVRAAUT   0.00  0.00                 0.00             0.01
## PAANHANG -0.11 -0.64                -0.23             1.43
## PTRACTOR  2.90 -0.42                 2.78             1.67
## PWERKT    0.00  0.00                 0.00             0.02
## PBROM    10.21 -4.88                 9.09             3.20
## PLEVEN   -0.59  2.00                -0.03             4.75
## PPERSONG  0.59  0.00                 0.59             0.20
## PGEZONG   0.48  3.29                 1.54             1.99
## PWAOREG   2.12  2.41                 2.76             1.71
## PBRAND    2.71  7.93                 5.23            19.45
## PZEILPL  -0.46 -1.00                -0.64             0.35
## PPLEZIER  8.32 11.49                11.86             5.87
## PFIETS    1.34  0.34                 1.39             3.21
## PINBOED   1.39 -2.10                 0.96             1.36
## PBYSTAND  1.40  2.94                 2.19             4.04
## AWAPART   3.75  2.92                 4.58             8.12
## AWABEDR  -0.18  1.25                 0.13             0.97
## AWALAND   0.91  0.58                 1.04             0.34
## APERSAUT -5.30 12.74                -1.66            17.07
## ABESAUT   0.37 -0.99                 0.18             0.57
## AMOTSCO  -1.75  1.38                -1.18             3.39
## AVRAAUT   0.00  0.00                 0.00             0.00
## AAANHANG  3.01  0.22                 2.97             1.24
## ATRACTOR  1.63 -3.18                 0.55             0.97
## AWERKT    1.00  0.00                 1.00             0.02
## ABROM     8.48 -1.76                 8.01             2.37
## ALEVEN   -4.25  2.43                -3.21             5.77
## APERSONG -0.80  1.00                -0.64             0.20
## AGEZONG  -1.02  0.87                -0.66             1.17
## AWAOREG   2.43  1.70                 2.75             1.55
## ABRAND    3.47  0.13                 3.55             8.15
## AZEILPL   0.31 -1.00                -0.02             0.19
## APLEZIER  9.92 11.51                12.61             5.27
## AFIETS    2.41  0.15                 2.28             4.41
## AINBOED   2.15 -0.27                 2.01             1.09
## ABYSTAND  2.87  3.14                 3.79             3.29
```

```r
ticData.RF.pred <- predict(ticData.RF, newdata = ticDataTest[-86], type = "prob")
```

### Building a Logistic Regression Model Using Variables from forward/backward variable selection


```r
ticDataLogitModel.FB.Var <- glm(data = ticDataTraining, CARAVAN ~ 
                             MRELGE +
                             MOPLLAAG +
                             MBERBOER +
                             MKOOPKLA +
                             PWALAND +
                             PWAPART +
                             PPERSAUT +
                             PBRAND +
                             APLEZIER +
                             ABYSTAND,
                         family = binomial)

summary(ticDataLogitModel.FB.Var)
```

```
## 
## Call:
## glm(formula = CARAVAN ~ MRELGE + MOPLLAAG + MBERBOER + MKOOPKLA + 
##     PWALAND + PWAPART + PPERSAUT + PBRAND + APLEZIER + ABYSTAND, 
##     family = binomial, data = ticDataTraining)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.5394  -0.3801  -0.2600  -0.1810   3.1463  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -4.64557    0.32427 -14.326  < 2e-16 ***
## MRELGE       0.11724    0.03409   3.439 0.000584 ***
## MOPLLAAG    -0.10254    0.02893  -3.545 0.000393 ***
## MBERBOER    -0.19961    0.07603  -2.625 0.008655 ** 
## MKOOPKLA     0.06913    0.03390   2.039 0.041434 *  
## PWALAND     -0.34127    0.18259  -1.869 0.061623 .  
## PWAPART      0.11602    0.07226   1.606 0.108340    
## PPERSAUT     0.22713    0.02397   9.475  < 2e-16 ***
## PBRAND       0.14865    0.03831   3.880 0.000105 ***
## APLEZIER     2.04064    0.37439   5.451 5.02e-08 ***
## ABYSTAND     0.53728    0.29867   1.799 0.072033 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2635.5  on 5821  degrees of freedom
## Residual deviance: 2335.5  on 5811  degrees of freedom
## AIC: 2357.5
## 
## Number of Fisher Scoring iterations: 6
```

```r
anova(ticDataLogitModel.FB.Var, test = "Chisq")
```

```
## Analysis of Deviance Table
## 
## Model: binomial, link: logit
## 
## Response: CARAVAN
## 
## Terms added sequentially (first to last)
## 
## 
##          Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
## NULL                      5821     2635.5              
## MRELGE    1   30.780      5820     2604.8 2.890e-08 ***
## MOPLLAAG  1   39.989      5819     2564.8 2.554e-10 ***
## MBERBOER  1   13.470      5818     2551.3 0.0002424 ***
## MKOOPKLA  1    7.598      5817     2543.7 0.0058429 ** 
## PWALAND   1    0.997      5816     2542.7 0.3179563    
## PWAPART   1   46.805      5815     2495.9 7.842e-12 ***
## PPERSAUT  1  115.138      5814     2380.8 < 2.2e-16 ***
## PBRAND    1   16.372      5813     2364.4 5.204e-05 ***
## APLEZIER  1   25.977      5812     2338.4 3.454e-07 ***
## ABYSTAND  1    2.936      5811     2335.5 0.0866288 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
ticData.Logit.FB.Pred <- predict(ticDataLogitModel.FB.Var, newdata = ticDataTest[-86], type = "response")
```

### Logit Model 2 Using Variable importance table from Random Forest Classifier


```r
ticDataLogitModel.RF.Var <- glm(data = ticDataTraining, CARAVAN ~ 
#                             MOSTYPE +
                             MKOOPKLA +
                             PBRAND +
                             APERSAUT +
                             PWAPART +
                             PPERSAUT,
                         family = binomial)

summary(ticDataLogitModel.RF.Var)
```

```
## 
## Call:
## glm(formula = CARAVAN ~ MKOOPKLA + PBRAND + APERSAUT + PWAPART + 
##     PPERSAUT, family = binomial, data = ticDataTraining)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.8015  -0.3997  -0.2735  -0.1938   3.0681  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -4.88049    0.19224 -25.388  < 2e-16 ***
## MKOOPKLA     0.18290    0.02837   6.448 1.13e-10 ***
## PBRAND       0.10409    0.03491   2.982  0.00287 ** 
## APERSAUT     0.06535    0.16909   0.387  0.69911    
## PWAPART      0.19327    0.06721   2.876  0.00403 ** 
## PPERSAUT     0.21615    0.04048   5.339 9.32e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2635.5  on 5821  degrees of freedom
## Residual deviance: 2407.5  on 5816  degrees of freedom
## AIC: 2419.5
## 
## Number of Fisher Scoring iterations: 6
```

```r
anova(ticDataLogitModel.RF.Var, test = "Chisq")
```

```
## Analysis of Deviance Table
## 
## Model: binomial, link: logit
## 
## Response: CARAVAN
## 
## Terms added sequentially (first to last)
## 
## 
##          Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
## NULL                      5821     2635.5              
## MKOOPKLA  1   52.905      5820     2582.6 3.501e-13 ***
## PBRAND    1   41.235      5819     2541.4 1.350e-10 ***
## APERSAUT  1   88.456      5818     2452.9 < 2.2e-16 ***
## PWAPART   1   13.476      5817     2439.5 0.0002416 ***
## PPERSAUT  1   31.955      5816     2407.5 1.578e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
ticData.Logit.RF.Pred <- predict(ticDataLogitModel.RF.Var, newdata = ticDataTest[-86], type = "response")
```

### Comparing the Classifier Models:


```r
c.legend <- c("Decision Tree AUC = ", 
              "Random Forest AUC = ", 
              "Naive Bayes AUC = ",
              "SVM Linear AUC = ",
              "SVM Radial AUC = ",
              "Logit with Fwd/Back Variable Selection AUC = ",
              "Logit with Random Forest Variable Selection AUC = ")

# ROC For Decision Tree

pred <- prediction(ticDataTest.PredictTree[,2], ticDataTest$CARAVAN)
perf <- performance(pred, "tpr", "fpr")
plot(perf, col = "red", lwd = 2)
c.legend[1] <- paste(c.legend[1], round((performance(pred, 'auc')@y.values)[[1]],3))

# ROC For Random Forest

pred <- prediction(ticData.RF.pred[,2], ticDataTest$CARAVAN)
perf <- performance(pred, "tpr", "fpr")
plot(perf, add = TRUE, col = "green", lwd = 2)
c.legend[2] <- paste(c.legend[2], round((performance(pred, 'auc')@y.values)[[1]],3))

#ROC For Naive Bayes

pred <- prediction(ticData.NB.pred[,2], ticDataTest$CARAVAN)
perf <- performance(pred, "tpr", "fpr")
plot(perf, add = TRUE, col = "blue", lwd = 2)
c.legend[3] <- paste(c.legend[3], round((performance(pred, 'auc')@y.values)[[1]],3))

#ROC For SVM Linear Kernel

pred <- prediction(attr(ticData.SVM.Linear.Pred, "probabilities")[,2], ticDataTest$CARAVAN)
perf <- performance(pred, "tpr", "fpr")
plot(perf, add = TRUE, col = "purple", lwd = 2)
c.legend[4] <- paste(c.legend[4], round((performance(pred, 'auc')@y.values)[[1]],3))

#ROC For SVM Radial Kernel

pred <- prediction(attr(ticData.SVM.Radial.Pred, "probabilities")[,2], ticDataTest$CARAVAN)
perf <- performance(pred, "tpr", "fpr")
plot(perf, add = TRUE, col = "black", lwd = 2)
c.legend[5] <- paste(c.legend[5], round((performance(pred, 'auc')@y.values)[[1]],3))

#ROC For Logit with Forward/Backward variable selection

pred <- prediction(ticData.Logit.FB.Pred, ticDataTest$CARAVAN)
perf <- performance(pred, "tpr", "fpr")
plot(perf, add = TRUE, col = "cyan", lwd = 2)
c.legend[6] <- paste(c.legend[6], round((performance(pred, 'auc')@y.values)[[1]],3))

#ROC For Logit with Random Forest variable selection

pred <- prediction(ticData.Logit.RF.Pred, ticDataTest$CARAVAN)
perf <- performance(pred, "tpr", "fpr")
plot(perf, add = TRUE, col = "pink", lwd = 2)
c.legend[7] <- paste(c.legend[7], round((performance(pred, 'auc')@y.values)[[1]],3))

legend("bottomright", 
       c.legend, 
       lty = c(1,1,1,1,1,1,1), 
       lwd = c(2,2,2,2,2,2,2), 
       col = c("red", "green", "blue", "purple", "black", "cyan", "pink"),
       pch=21, cex = 1)
```

![](CapstoneProject_files/figure-html/unnamed-chunk-16-1.png)
