# Libraries loading
library(tidyverse)
library(dplyr)
library(moments)
library(factoextra)
library(FactoMineR)

# Data loading
data <- read.csv(file="./Data/dane_mgr.csv", header=TRUE, sep=";")


# Data preprocessing
str(data) # data structure infromation
dim(data) # data size: 41 rows, 113 columns
names(data) # column names

# Removing redundant columns
data <- select(data, !(starts_with("Choroby")))

# Missing values checking
anyNA(data, recursive = FALSE) # yes
sum(is.na(data)) # 120
sort(colSums(is.na(data)> 0), decreasing = T) # column names with na values (descending order)

# NA's filling (with assumption that an economist works in a big city)
data$MiastoPowyzej50tys[37] = TRUE


# ---------------------------------------------------------------------------------------------- #
# 1. BASIC STATISTICS DESCRIBING GROUP OF ALL PATIENTS: age, living place, profession, education
# ---------------------------------------------------------------------------------------------- #

# 1.1 WITHOUT SPLIT ON GENDER

    # 1.1.1 AGE

bootAge <- function(my_sample, R){
  n = length(my_sample)
  boot_mean = numeric(R)
  boot_sd = numeric(R)
  boot_median = numeric(R)
  boot_min = numeric(R)
  boot_max = numeric(R)
  
  for(i in 1:R){
    boot_sample = sample(my_sample, n, replace = T)
    boot_mean[i] = mean(boot_sample)
    boot_sd[i] = sd(boot_sample)
    boot_median[i] = median(boot_sample)
    boot_min[i] = min(boot_sample)
    boot_max[i] = max(boot_sample)
  }
  return( list(boot_mean = mean(boot_mean), boot_sd = mean(boot_sd), boot_median = mean(boot_median), boot_min = mean(boot_min), boot_max = mean(boot_max)) )
}

bootAge(data$Wiek, R = 150)

    # 1.1.1 LIVING PLACE

bootLivingPlace <- function(my_sample, R){
  n = nrow(my_sample)
  Village = numeric(R)
  TownBelow_50k = numeric(R)
  TownAbove_50 = numeric(R)
  
  for(i in 1:R){
    boot_sample = sample(my_sample, n, replace = T)
    Village[i] = sum(!is.na(boot_sample$Wies))
    TownBelow_50k[i] = sum(!is.na(boot_sample$MiastoPonizej50tys))
    TownAbove_50[i] = sum(!is.na(boot_sample$MiastoPowyzej50tys))
  }
  return( list(Village = mean(Village)*R, TownBelow_50k = mean(TownBelow_50k)*R, TownAbove_50 = mean(TownAbove_50)*R) )
}

bootLivingPlace(data %>% select(Wies, MiastoPonizej50tys, MiastoPowyzej50tys), R = 150)

    # 1.1.1 PROFESSION

bootProfession <- function(my_sample, R){
  n = length(my_sample)
  Pupil = numeric(R)
  Student = numeric(R)
  White_collar = numeric(R)
  Blue_collar = numeric(R)
  
  for(i in 1:R){
    boot_sample = sample(my_sample, n, replace = T)
    Pupil[i] = length(boot_sample[boot_sample == 'U'])
    Student[i] = length(boot_sample[boot_sample == 'S'])
    White_collar[i] = length(boot_sample[boot_sample == 'PU'])
    Blue_collar[i] = length(boot_sample[boot_sample == 'PF'])
  }
  return( list(Pupil = Pupil, Student = Student, White_collar = White_collar, Blue_collar = Blue_collar) )
}

bootProfession(data['KategoriaZawodowa'], R = 150)

    # 1.1.1 EDUCATION

bootEducation <- function(my_sample, R){
  n = length(my_sample)
  Elementary = numeric(R)
  Secondary = numeric(R)
  Higher = numeric(R)
  
  for(i in 1:R){
    boot_sample = sample(my_sample, n, replace = T)
    Elementary[i] = length(boot_sample[boot_sample == 'podstawowe'])
    Secondary[i] = length(boot_sample[boot_sample == 'średnie'])
    Higher[i] = length(boot_sample[boot_sample == 'wyższe'])
  }
  return( list(Elementary = Elementary, Secondary = Secondary, Higher = Higher) )
}

bootEducation(data['Wyksztalcenie'], R = 150)


# 1.2 WITH SPLIT ON GENDER

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
chisq.test(livingPlace_matrix) # p-value = 0.3078


job <- data %>% 
    group_by(Plec) %>% 
    count(KategoriaZawodowa) %>% 
    mutate(procent = n/sum(n)*100)

job_matrix <- matrix(rbind(c(filter(job, Plec == "K")$n), c(filter(job, Plec == "M")$n)),nrow=2,ncol=4)
chisq.test(job_matrix) # p-value = 0.1834

education <- data %>% 
    group_by(Plec) %>% 
    count(Wyksztalcenie) %>% 
    mutate(procent = n/sum(n)*100)

edu_matrix <- matrix(rbind(c(filter(education, Plec == "K")$n), c(filter(education, Plec == "M")$n)),nrow=2,ncol=4)
chisq.test(edu_matrix) # p-value = 0.2699

## No statistically significant difference was observed in terms of analyzed features (p > 0.05).

