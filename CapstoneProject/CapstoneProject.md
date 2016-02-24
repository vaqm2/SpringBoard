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
```

## Data Cleaning


```r
colNames <- variableInfo %>% select(Name) %>% unlist()
names(ticDataTraining) <- colNames
names(ticDataTest) <- colNames[1:85]
ticDataTest$CARAVAN <- NA
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
ticData <- ticData %>% mutate(thirdPartyInsuranceLevel = AWAPART * PWAPART)
ticData <- ticData %>% mutate(thirdPartyInsuranceLevelFirm = AWABEDR * PWABEDR)
ticData <- ticData %>% mutate(thirdPartyInsuranceLevelAgriculture = AWALAND * PWALAND)
ticData <- ticData %>% mutate(carPolicyLevel = APERSAUT * PPERSAUT)
ticData <- ticData %>% mutate(vanPolicyLevel = ABESAUT * PBESAUT)
ticData <- ticData %>% mutate(scooterPolicyLevel = AMOTSCO * PMOTSCO)
ticData <- ticData %>% mutate(lorryPolicyLevel = AVRAAUT * PVRAAUT)
ticData <- ticData %>% mutate(trailerPolicyLevel = AAANHANG * PAANHANG)
ticData <- ticData %>% mutate(tractorPolicyLevel = ATRACTOR * PTRACTOR)
ticData <- ticData %>% mutate(agriMachinePolicyLevel = AWERKT * PWERKT)
ticData <- ticData %>% mutate(mopedPolicyLevel = ABROM * PBROM)
ticData <- ticData %>% mutate(lifeInsuranceLevel = ALEVEN * PLEVEN)
ticData <- ticData %>% mutate(pvtAccidentInsuranceLevel = APERSONG * PPERSONG)
ticData <- ticData %>% mutate(famAccidentInsuranceLevel = AGEZONG * PGEZONG)
ticData <- ticData %>% mutate(disabilityInsuranceLevel = AWAOREG * PWAOREG)
ticData <- ticData %>% mutate(fireInsuranceLevel = ABRAND * PBRAND)
ticData <- ticData %>% mutate(surfInsuranceLevel = AZEILPL * PZEILPL)
ticData <- ticData %>% mutate(boatInusaranceLevel = APLEZIER * PPLEZIER)
ticData <- ticData %>% mutate(bikeInsuranceLevel = AFIETS * PFIETS)
ticData <- ticData %>% mutate(propertyInsuranceLevel = AINBOED * PINBOED)
ticData <- ticData %>% mutate(SocSecInsurancelevel = ABYSTAND * PBYSTAND)

ticDataDerived <- ticData[,c(1:43,86:109)]

ticDataDerived <- ticDataDerived %>% 
    filter(!is.na(CARAVAN)) %>%
    melt(id.vars = c("MOSTYPE", "MOSTYPE2", "MOSHOOFD", "MOSHOOFD2", "CARAVAN"))

ticDataDerived <- ticDataDerived %>% 
    group_by(variable) %>% 
    mutate(num.Caravan.Buyers = sum(CARAVAN))

glimpse(ticDataDerived)
```

```
## Observations: 360,964
## Variables: 8
## $ MOSTYPE            (int) 33, 37, 37, 9, 40, 23, 39, 33, 33, 11, 10, ...
## $ MOSTYPE2           (fctr) Lowerclasslargefamilies, Mixedsmalltowndwe...
## $ MOSHOOFD           (int) 8, 8, 8, 3, 10, 5, 9, 8, 8, 3, 3, 3, 8, 10,...
## $ MOSHOOFD2          (fctr) Familywithgrownups, Familywithgrownups, Fa...
## $ CARAVAN            (int) 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0...
## $ variable           (fctr) MAANTHUI, MAANTHUI, MAANTHUI, MAANTHUI, MA...
## $ value              (int) 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1...
## $ num.Caravan.Buyers (int) 348, 348, 348, 348, 348, 348, 348, 348, 348...
```

```r
summary(ticDataDerived$num.Caravan.Buyers)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     348     348     348     348     348     348
```

```r
knit_exit()
```






