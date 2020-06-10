# Libraries loading
library(tidyverse)
library(dplyr)
library(moments)

# Data loading
data <- read.csv(file=choose.files(), header=TRUE, sep=";")

# Data preprocessing
str(data) # data structure infromation
dim(data) # data size: 41 rows, 113 columns
names(data) # column names

# Missing values checking
anyNA(data, recursive = FALSE) # yes
sum(is.na(data)) # 120
sort(colSums(is.na(data)> 0), decreasing = T) # column names with na values (descending order)

# NA's filling (with assumption that an economist works in a big city)
data$MiastoPowyzej50tys[37] = TRUE

n = nrow(data) # number of patients = 41


# ---------------------------------------------------------------------------------------------- #
# 1. BASIC STATISTICS DESCRIBING GROUP OF ALL PATIENTS: age, living place, profession, education
# ---------------------------------------------------------------------------------------------- #

  # 1.1 WITHOUT SPLIT ON GENDER

data %>%
  summarise( n=n(),
             AvgAge = mean(Wiek, na.rm=T),
             Village = sum(!is.na(Wies)/n*100), 
             TownBelow_50k = sum(!is.na(MiastoPonizej50tys))/n*100,
             TownAbove_50k = sum(!is.na(MiastoPowyzej50tys)))/n*100

summary(data$Wiek) # summary for age
# binwidth=0
# require(ggplot2)
# if ( is.null(binwidth) )
#   binwidth = diff(range(data$Wiek))/30
# p = ggplot(data.frame(x=data$Wiek),aes(x=x)) +
#   geom_histogram(aes(y=..density..),binwidth=binwidth) +
#   geom_density(color="red")
# plot(p)

data %>% 
  count(KategoriaZawodowa) %>% 
  mutate(procent = n/sum(n)*100)

data %>% 
  count(Wyksztalcenie) %>% 
  mutate(procent = n/sum(n)*100)


  # 1.2 WITH SPLIT ON GENDER

age_m <- filter(data, Plec=='M')$Wiek
age_f <- filter(data, Plec=='K')$Wiek

shapiro.test(age_m) # p-value = 0.181
shapiro.test(age_f) # p-value = 1.207e-05

# Wilcoxon rank sum test —ranksum— testuje hipotezę zerową, że dwie próby X i Y, które ze sobą porównujemy pochodzą z populacji o takiej samej medianie. Próby X i Y nie są sparowane.
wilcox.test(age_m, age_f) # p-value = 0.3147

livingPlace <- data %>%
            group_by(Plec) %>% 
            summarise( n=n(),
                       AvgAge = mean(Wiek, na.rm=T),
                       StdAge = sd(Wiek, na.rm=T),
                       MeAge = median(Wiek, na.rm=T),
                       MinAge = min(Wiek, na.rm=T),
                       MaxAge = max(Wiek, na.rm=T),
                       Village = sum(!is.na(Wies)), 
                       TownBelow_50k = sum(!is.na(MiastoPonizej50tys)),
                       TownAbove_50k = sum(!is.na(MiastoPowyzej50tys)),
                       Sum = Village + TownBelow_50k + TownAbove_50k) %>% 
            mutate(Village_percent = Village/Sum*100, TownBelow_percent = TownBelow_50k/Sum*100,
                   TownAbove_percent = TownAbove_50k/Sum*100)

livingPlace_matrix <- matrix(rbind(c(5,2,22), c(0,1,11)),nrow=2,ncol=3)
# H0 : there is no relationship between sex and place of residence in the studied population,
# H1 : there is a relationship between sex and place of residence in the studied population.
fisher.test(livingPlace_matrix) # p-value = 0.438

job <- data %>% 
  group_by(Plec) %>% 
  count(KategoriaZawodowa) %>% 
  mutate(procent = n/sum(n)*100)

job_matrix <- matrix(rbind(c(filter(job, Plec == "K")$n), c(filter(job, Plec == "M")$n)),nrow=2,ncol=4)
# H0 : there is no relationship between gender and performed profession in the studied population,
# H1 : there is a relationship between gender and performed profession in the studied population.
fisher.test(job_matrix) # p-value = 0.1782

education <- data %>% 
  group_by(Plec) %>% 
  count(Wyksztalcenie) %>% 
  mutate(procent = n/sum(n)*100)

edu_matrix <- matrix(rbind(c(filter(education, Plec == "K")$n), c(filter(education, Plec == "M")$n)),nrow=2,ncol=4)
# H0 : there is no relationship between gender and education in the studied population,
# H1 : there is a relationship between gender and education in the studied population.
fisher.test(edu_matrix) # p-value = 0.2121

## No statistically significant difference was observed in terms of analyzed features (p > 0.05).



# --------------------------------------------------------------- #
# 2. STATISTICS FOR VSC MEASUREMENTS (volatile sulphur compounds)
# --------------------------------------------------------------- #

  # 2.1 VSC BEFORE TONSILLECTOMY

VSC_before <- select(data, Plec, Wiek, SredniaPomiar1)
summary(VSC_before)
# binwidth=0
# require(ggplot2)
# if ( is.null(binwidth) )
#   binwidth = diff(range(VSC_before$SredniaPomiar1))/30
# p = ggplot(data.frame(x=VSC_before$SredniaPomiar1),aes(x=x)) +
#   geom_histogram(aes(y=..density..),binwidth=binwidth) +
#   geom_density(color="red")
# plot(p)

    # 2.1.1 STATISTIC TESTS WITH SPLIT ON GENDER

VSC_before %>%
  group_by(Plec) %>% 
  summarise(n = n(), 
            AvgAge = mean(Wiek, na.rm=T), 
            AvgVSC = mean(SredniaPomiar1, na.rm=T), 
            Sd = sd(SredniaPomiar1), 
            Median = median(SredniaPomiar1), 
            Min = min(SredniaPomiar1), 
            Max = max(SredniaPomiar1))

VSC_before_f <- filter(VSC_before, Plec=="K")$SredniaPomiar1
VSC_before_m <- filter(VSC_before, Plec=="M")$SredniaPomiar1

shapiro.test(VSC_before_f) #  p-value = 0.2542
shapiro.test(VSC_before_m) # p-value = 0.3716

var.test(VSC_before_f, VSC_before_m) # p-value = 0.01254, ratio of variances 0.313
## Samples of VSC measurements for both sexes are normally distributed , however the assumption of homogeneity of variance is not satisfied --> non-parametric test

wilcox.test(VSC_before_f, VSC_before_m) # p-value = 0.5378 --> no difference between sample means


    # 2.1.1 STATISTIC TESTS AFTER SPLITTING INTO GROUPS  

    # Patients were splitted into research groups A - D according to the results of measurements of VSC 
    # concentration in exhaled air (before performing the operation) and the literature thresholds.

VSC_before <- mutate(VSC_before, Grupa = SredniaPomiar1)

VSC_before$Grupa[VSC_before$SredniaPomiar1 <= 100] = "A"
VSC_before$Grupa[VSC_before$SredniaPomiar1 > 100 & VSC_before$SredniaPomiar1 <= 180] = "B"
VSC_before$Grupa[VSC_before$SredniaPomiar1 > 180 & VSC_before$SredniaPomiar1 <= 250] = "C"
VSC_before$Grupa[VSC_before$SredniaPomiar1 > 250] = "D"


# TWO GROUPS --> See: Piechart no. 1
# MORE GROUPS --> See: Piechart no. 2
# See: Histogram of the distribution of VSC mean concentration in exhaled air in patients before surgery, calculated on the basis of 3 measurements (Ia, Ib, Ic). Based on the average, we assess the severity of halitosis.


# The Fisher-Freeman-Halton Exact test 

contingency.table <- table(VSC_before$Grupa, VSC_before$Plec)
observed <- t(matrix(c(9, 19,  1, 5,  5, 2),nrow=3,ncol=2))
fisher.test(observed) # p-value = 0.1793
## No statistically significant difference was observed in terms of analyzed features (p > 0.05).


    # Distributions and homogenity check for features: Age ang Sex (in groups).

shapiro.test(filter(VSC_before, Grupa == "A")$Wiek) # p-value = 0.4122
shapiro.test(filter(VSC_before, Grupa == "A", Plec == "K")$Wiek) # p-value = 0.8672
shapiro.test(filter(VSC_before, Grupa == "A", Plec == "M")$Wiek) # p-value = 0.05023

shapiro.test(filter(VSC_before, Grupa == "B") $Wiek) # p-value = 7.823e-05 < 0.05
shapiro.test(filter(VSC_before, Grupa == "B", Plec == "K")$Wiek) # p-value = 0.0002648
shapiro.test(filter(VSC_before, Grupa == "B", Plec == "M")$Wiek) # p-value = 0.2866

shapiro.test(filter(VSC_before, Grupa == "C") $Wiek) # p-value = 0.253
shapiro.test(filter(VSC_before, Grupa == "C", Plec == "K")$Wiek) # n<3
shapiro.test(filter(VSC_before, Grupa == "C", Plec == "M")$Wiek) # n<3

## "Age" variable is normally distributed in A and C group (when testing both genders) and in B group (when testing males).
## The distribution is not normal among women in group B and generally in B group. The reason for that are probably two outliers (See: Age distributions in groups plot).

    
    # Bartlett test of homogeneity of variances 

bartlett.test(VSC_before$Wiek, g=VSC_before$Grupa) # p-value = 0.116
## The variances in each of the groups are the same.


    # Comparison between subgroups

VSC_before %>%
  group_by(Grupa) %>% 
  summarise(n = n(), 
            AvgAge = mean(Wiek, na.rm=T),
            AvgVSC = mean(SredniaPomiar1, na.rm=T),  
            Std = sd(SredniaPomiar1), 
            Median = median(SredniaPomiar1), 
            Min = min(SredniaPomiar1, na.rm=T), 
            Max = max(SredniaPomiar1, na.rm=T)) 

avgVSC_A <- mean(VSC_before$SredniaPomiar1[VSC_before$Grupa == "A"])
avgVSC_B <- mean(VSC_before$SredniaPomiar1[VSC_before$Grupa == "B"])
avgVSC_C <- mean(VSC_before$SredniaPomiar1[VSC_before$Grupa == "C"])

avgVSC_C/avgVSC_A*100 
avgVSC_C/avgVSC_B*100
## The average level of VSC is the highest in group C - it is 326.08% higher than in Group A and 155.02% higher than in Group B.
## See: Boxplots for VSC (before tonsillectomy)



  # 2.2 VSC AFTER TONSILLECTOMY

VSC_after <- select(data, Plec, Wiek, SredniaPomiar2)
summary(VSC_after)

binwidth=0
require(ggplot2)
if ( is.null(binwidth) )
  binwidth = diff(range(VSC_after$SredniaPomiar2))/30
p = ggplot(data.frame(x=VSC_after$SredniaPomiar2),aes(x=x)) +
  geom_histogram(aes(y=..density..),binwidth=binwidth) +
  geom_density(color="red")
# plot(p)

    # 2.2.1 STATISTIC TESTS WITH SPLIT ON GENDER

VSC_after_f <- filter(VSC_after, Plec=="K")$SredniaPomiar2
vsc_after_m <- filter(VSC_after, Plec=="M")$SredniaPomiar2

shapiro.test(VSC_after_f) #  p-value = 0.3131
shapiro.test(vsc_after_m) # p-value = 0.005966 < 0.05
## Samples of VSC measurements for male are not normally distributed --> non-parametric test

wilcox.test(VSC_after_f, vsc_after_m, paired=F, exact=F) #  p-value = 0.8185 --> no difference between sample means



    # 2.2.1 STATISTIC TESTS AFTER SPLITTING INTO GROUPS

VSC_after$Grupa[VSC_after$SredniaPomiar2 <= 100] = "A"
VSC_after$Grupa[VSC_after$SredniaPomiar2 > 100 & VSC_after$SredniaPomiar2 <= 180] = "B"
VSC_after$Grupa[VSC_after$SredniaPomiar2 > 180 & VSC_after$SredniaPomiar2 <= 250] = "C"
VSC_after$Grupa[VSC_after$SredniaPomiar2 > 250] = "D"

# Correct results (<= 100 ppb VSC)
normal_after <- sum(VSC_after$Grupa == "A") # 40
normal_after_percent <- round(normal_after*100/n, 2) # 97.56 %

# Incorrect results - halitosis (> 100 ppb VSC)
above_after <-  n - normal_after # 1
above_after_percent <- round(above_after*100/n, 2) # 2.44 %

filter(VSC_after, Grupa != "A") # B (100 - 180 ppb VSC)
# The only person with light form of halitosis after tonsillectomy is a 29-year old man with 113 ppb of VSC.


VSC_after %>%
  group_by(Grupa) %>% 
  summarise(n = n(), 
            AvgAge = mean(Wiek, na.rm=T),
            AvgVSC = mean(SredniaPomiar2, na.rm=T),  
            Sdt = sd(SredniaPomiar2), 
            Median = median(SredniaPomiar2), 
            Min = min(SredniaPomiar2, na.rm=T), 
            Max = max(SredniaPomiar2, na.rm=T)) 

as.data.frame(VSC_after)

## See: Boxplots for VSC (after tonfillectomy)



  # 2.3. COMPARISON OF VSC RESULTS - BEFORE AND AFTER TONSILLECTOMY

shapiro.test(data$SredniaPomiar1) # p-value = 0.5822
shapiro.test(data$SredniaPomiar2) # p-value = 0.04493
# The average VSC measurements taken before surgery have a normal distribution, and after the operation deviate from it.

# hist(data$SredniaPomiar2)
skewness(data$SredniaPomiar2) # 0.7873434, right-skewed distribution --> Box-Cox transformation
skewness(sqrt(data$SredniaPomiar2)) # 0.01777133
shapiro.test(sqrt(data$SredniaPomiar2)) # p-value = 0.5834 --> ok

data$SredniaPomiar2norm <- sqrt(data$SredniaPomiar2)

t.test(data$SredniaPomiar1, data$SredniaPomiar2norm, paired=T) # p-value < 2.2e-16
# We have basis for rejecting the null hypothesis that the means of two samples of dependent distributions do not differ from each other. The difference is 106.5 ppb.

# ---------------------------------------------------------------------------------------------- #
# 3. CORRELATIONS: age, living place, profession, education
# ---------------------------------------------------------------------------------------------- #

library(PerformanceAnalytics)

# Living place encoding
livingPlace <- numeric(nrow(data))
livingPlace[data[,'Wies'] == TRUE] <- 1
livingPlace[data[,'MiastoPowyzej50tys'] == TRUE] <- 2
livingPlace[data[,'MiastoPonizej50tys'] == TRUE] <- 3
# data <- cbind(data, livingPlace)

profession <- numeric(nrow(data))
profession[data[,'KategoriaZawodowa'] == "U"] <- 1
profession[data[,'KategoriaZawodowa'] == "S"] <- 2
profession[data[,'KategoriaZawodowa'] == "PU"] <- 3
profession[data[,'KategoriaZawodowa'] == "PF"] <- 4
# data <- cbind(data, profession)

education <- numeric(nrow(data))
education[data[,'Wyksztalcenie'] == "podstawowe"] <- 1
education[data[,'Wyksztalcenie'] == "średnie"] <- 2
education[data[,'Wyksztalcenie'] == "wyższe"] <- 3
# data <- cbind(data, education)

sex <- numeric(nrow(data))
sex[data[,'Plec'] == "K"] <- 0
sex[data[,'Plec'] == "M"] <- 1
# data <- cbind(data, sex)


corrs <- cbind(select(data, Wiek, SredniaPomiar1), sex, profession, education, livingPlace)
corrs2 <- cbind(select(data, Wiek, SredniaPomiar2), sex, profession, education, livingPlace)

chart.Correlation(corrs, method="pearson")
cor.test(data$SredniaPomiar1, livingPlace)
chart.Correlation(corrs2, method="pearson")
# ---------------------------------------------------------------------- #
# 4. PRINCIPAL COMPONENT ANALYSIS --> PCA.R

# 5. MICROBIOLOGICAL RESEARCH --> microAnalysis.R

# 6. IMPACT OF AIR POLUTIONS IN A LIVING PLACE --> livingPlaceAnalysis.R
# ---------------------------------------------------------------------- #



