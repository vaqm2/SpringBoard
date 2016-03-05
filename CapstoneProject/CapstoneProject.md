# CapstoneProject
Vivek Appadurai  
February 20, 2016  

## Loading Libraries


```r
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(knitr)
library(gridExtra)
library(ROCR)
library(e1071)
library(brglm)
library(logistf)
```

## Reading Data


```r
variableInfo <- read.table("Names.txt", 
                           header = T, 
                           sep = "\t", 
                           fill = NA, 
                           quote = "",
                           stringsAsFactors = FALSE)
L0 <- read.table("L0.txt", header = T, sep = "\t", fill = NA, quote = "")
L1 <- read.table("L1.txt", header = F, sep = "\t", fill = NA, quote = "")
L2 <- read.table("L2.txt", header = F, sep = "\t", fill = NA, quote = "")
L3 <- read.table("L3.txt", header = F, sep = "\t", fill = NA, quote = "")
L4 <- read.table("L4.txt", header = F, sep = "\t", fill = NA, quote = "")
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
colNames <- variableInfo %>% select(Name) %>% unlist()
names(ticDataTraining) <- colNames
ticDataTraining$Set <- "Training"
ticDataTest$CARAVAN <- targets$V1
names(ticDataTest) <- colNames
ticDataTest$Set <- "Test"
ticData <- rbind(ticDataTraining, ticDataTest)
ticData <- left_join(ticData, L0, by = c("MOSTYPE"= "Value"))
ticData <- ticData %>% rename(MOSTYPE2 = Label)
names(L2) <- c("MOSHOOFD","MOSHOOFD2")
ticData <- left_join(ticData, L2)
```

```
## Joining by: "MOSHOOFD"
```

```r
ticDataTraining <- ticData %>% filter(Set == "Training")
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

![](CapstoneProject_files/figure-html/unnamed-chunk-5-1.png)

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
    scale_x_continuous(breaks =seq(0, max(ticData$APERSAUT), 1))
```

![](CapstoneProject_files/figure-html/unnamed-chunk-6-1.png)

The plot indicates that people who have a car policy tend to contribute significantly towards it.


```r
ggplot(ticDataTraining, aes(x = AWAPART, y = PWAPART, color = as.factor(CARAVAN))) +
    geom_jitter() + 
    xlab("Private Third Party Insurance") +
    ylab("Contributions") +
    theme_bw() +
    scale_x_continuous(breaks =seq(0, max(ticData$AWAPART), 1))
```

![](CapstoneProject_files/figure-html/unnamed-chunk-7-1.png)


```r
ggplot(ticDataTraining, aes(x = ABRAND, y = PBRAND, color = as.factor(CARAVAN))) +
    geom_jitter() + 
    xlab("Num. Fire Policies") +
    ylab("Contributions") +
    theme_bw() +
    scale_x_continuous(breaks =seq(0, max(ticData$ABRAND), 1))
```

![](CapstoneProject_files/figure-html/unnamed-chunk-8-1.png)

### Building a Logistic Regression Model


```r
ticDataTraining$CARAVAN <- as.factor(ticDataTraining$CARAVAN)
ticDataTest$CARAVAN <- as.factor(ticDataTest$CARAVAN)

ticDataLogitModel <- glm(data = ticDataTraining, CARAVAN ~ 
                             APERSAUT +
                             PPERSAUT +
                             ABRAND +
                             PBRAND +
                             AWAPART +
                             PWAPART +
                             MKOOPKLA +
                             MOPLLAAG +
                             MHKOOP +
                             MHHUUR +
                             MINKM30 +
#                             MAUT0 +
                             MRELGE +
#                             MBERARBO +
#                             MFALLEEN +
                             MAUT1,
                         family = binomial)

summary(ticDataLogitModel)
```

```
## 
## Call:
## glm(formula = CARAVAN ~ APERSAUT + PPERSAUT + ABRAND + PBRAND + 
##     AWAPART + PWAPART + MKOOPKLA + MOPLLAAG + MHKOOP + MHHUUR + 
##     MINKM30 + MRELGE + MAUT1, family = binomial, data = ticDataTraining)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.9091  -0.3890  -0.2687  -0.1815   3.1793  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  118.85665 2894.81441   0.041 0.967249    
## APERSAUT       0.04993    0.16892   0.296 0.767525    
## PPERSAUT       0.21377    0.04051   5.277 1.32e-07 ***
## ABRAND        -0.25190    0.24949  -1.010 0.312663    
## PBRAND         0.16191    0.06521   2.483 0.013027 *  
## AWAPART       -0.95645    0.75961  -1.259 0.207981    
## PWAPART        0.67934    0.38052   1.785 0.074213 .  
## MKOOPKLA       0.06225    0.03520   1.768 0.077017 .  
## MOPLLAAG      -0.10242    0.02944  -3.478 0.000504 ***
## MHKOOP       -13.73058  321.64605  -0.043 0.965950    
## MHHUUR       -13.74903  321.64605  -0.043 0.965904    
## MINKM30       -0.02673    0.03804  -0.703 0.482253    
## MRELGE         0.07666    0.03872   1.980 0.047717 *  
## MAUT1          0.08442    0.04175   2.022 0.043146 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2635.5  on 5821  degrees of freedom
## Residual deviance: 2364.4  on 5808  degrees of freedom
## AIC: 2392.4
## 
## Number of Fisher Scoring iterations: 15
```

```r
anova(ticDataLogitModel, test="Chisq")
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
## APERSAUT  1  105.360      5820     2530.2 < 2.2e-16 ***
## PPERSAUT  1   36.806      5819     2493.4 1.305e-09 ***
## ABRAND    1   19.910      5818     2473.5 8.115e-06 ***
## PBRAND    1   16.587      5817     2456.9 4.646e-05 ***
## AWAPART   1    9.296      5816     2447.6  0.002297 ** 
## PWAPART   1    5.165      5815     2442.4  0.023050 *  
## MKOOPKLA  1   38.962      5814     2403.4 4.322e-10 ***
## MOPLLAAG  1   15.882      5813     2387.6 6.741e-05 ***
## MHKOOP    1    3.880      5812     2383.7  0.048862 *  
## MHHUUR    1    5.352      5811     2378.3  0.020694 *  
## MINKM30   1    2.885      5810     2375.4  0.089422 .  
## MRELGE    1    6.952      5809     2368.5  0.008372 ** 
## MAUT1     1    4.110      5808     2364.4  0.042642 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
ticDataTrainingPrediction <- predict(ticDataLogitModel, type = "response")
summary(ticDataTrainingPrediction)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.00000 0.01916 0.03997 0.05977 0.08163 0.33850
```

```r
table(ticDataTraining$CARAVAN, ticDataTrainingPrediction > 0.14)
```

```
##    
##     FALSE TRUE
##   0  5042  432
##   1   224  124
```

```r
tapply(ticDataTrainingPrediction, ticDataTraining$CARAVAN, mean)
```

```
##          0          1 
## 0.05641357 0.11262110
```

```r
ticDataTraining$Prediction <- ticDataTrainingPrediction

ticDataTestPrediction <- predict(ticDataLogitModel, newdata = ticDataTest, type= "response")
table(ticDataTest$CARAVAN, ticDataTestPrediction > 0.1)
```

```
##    
##     FALSE TRUE
##   0  3162  600
##   1   133  105
```

```r
tapply(ticDataTestPrediction, ticDataTest$CARAVAN, mean)
```

```
##          0          1 
## 0.05639077 0.10525544
```

```r
ggplot(ticDataTraining, aes(x = CARAVAN, y = Prediction, fill = CARAVAN)) +
    geom_boxplot() +
    scale_y_continuous(breaks = seq(0,1,0.05)) +
    theme_bw()
```

![](CapstoneProject_files/figure-html/unnamed-chunk-9-1.png)
