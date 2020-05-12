# Libraries loading
library(tidyverse)
library(dplyr)
library(boot)


# Data loading
data <- read.csv(file=file.choose(), header=TRUE, sep=";")


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



# bootstrapping function to get the mean
boot.mean <- function(x,i){
    mean(x[i])
} 

# bootstrapping function to get the sd
boot.sd <- function(x,i){
    sd(x[i])
}

# bootstrapping function to get the median
boot.me <- function(x,i){
    median(x[i])
}

# bootstrapping function to get the spread
boot.min <- function(x,i){
    min(x[i])
}

boot.max <- function(x,i){
    max(x[i])
}

# bootstrapping function to count
boot.count <- function(x,i){
    sum(x[i], na.rm = TRUE)
}



# 1.1 WITHOUT SPLIT ON GENDER

    # 1.1.1 AGE

lapply(data['Wiek'], function(y){ 
    boot.mean <- boot(y, boot.mean, R = 150); 
    boot.sd <- boot(y, boot.sd, R = 150); 
    boot.me <- boot(y, boot.me, R = 150); 
    boot.min <- boot(y, boot.min, R = 150);
    boot.max <- boot(y, boot.max, R = 150);

    return(list(boot.mean = boot.mean, boot.sd = boot.sd, boot.me = boot.me, boot.min = boot.min, boot.max = boot.max))
})

    # 1.1.2 LIVING PLACE

bootLivingPlace <- data %>% select(Wies, MiastoPonizej50tys, MiastoPowyzej50tys)

lapply(data['Wies'], function(y){
    boot.count <- boot(y, boot.count, R = 150);
    return(boot.count)
})

lapply(data['MiastoPonizej50tys'], function(y){
    boot.count <- boot(y, boot.count, R = 150);
    return(boot.count)
})

lapply(data['MiastoPowyzej50tys'], function(y){
    boot.count <- boot(y, boot.count, R = 150);
    return(boot.count)
})


# 1.2 WITH SPLIT ON GENDER
    # 1.2.1 AGE

female <- filter(data, Plec=='K')
male <- filter(data, Plec=='M')

bootAgeFemale <- lapply(female['Wiek'], function(y){ 
    boot.mean <- boot(y, boot.mean, R = 150); 
    boot.sd <- boot(y, boot.sd, R = 150); 
    boot.me <- boot(y, boot.me, R = 150); 
    boot.min <- boot(y, boot.min, R = 150);
    boot.max <- boot(y, boot.max, R = 150);

    return(list(boot.mean = boot.mean, boot.sd = boot.sd, boot.me = boot.me, boot.min = boot.min, boot.max = boot.max))
})

bootAgeMale <- lapply(male['Wiek'], function(y){ 
    boot.mean <- boot(y, boot.mean, R = 150); 
    boot.sd <- boot(y, boot.sd, R = 150); 
    boot.me <- boot(y, boot.me, R = 150); 
    boot.min <- boot(y, boot.min, R = 150);
    boot.max <- boot(y, boot.max, R = 150);
    
    return(list(boot.mean = boot.mean, boot.sd = boot.sd, boot.me = boot.me, boot.min = boot.min, boot.max = boot.max))
})


# ----------------------
# To samo własną funkcją
# ----------------------

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
    return( list(boot_mean = mean(boot_mean), boot_sd = mean(boot_sd), boot_median = mean(boot_median), 
                 boot_min = mean(boot_min), boot_max = mean(boot_max)) )
}


# ------------------
# Porównanie wyników
# ------------------

# średni wiek kobiet - próba oryginalna
mean(female$Wiek) # 26.68966

# średni wiek kobiet - bootstrap z biblioteki boot
bootAgeFemale$Wiek$boot.mean$t0 # 26.68966


# średni wiek kobiet - bootstrap z własnej funkcji
bootAge(female$Wiek, R = 150)$boot_mean # 26.58138



# -------------------
# Dalsze postępowanie
# -------------------

shapiro.test(female$Wiek) # p-value = 1.207e-05
shapiro.test(male$Wiek) # p-value = 0.181

# The distribution of male's age is not normal --> non-parametric test

# H0: The women age median is equal to the men age median.
# H1: The women age median is not equal to the men age median.

wilcox.test(female$Wiek, male$Wiek, paired = FALSE) # p-value = 0.3147

wilcox.test(bootAgeFemale$Wiek$boot.mean$t0, bootAgeMale$Wiek$boot.mean$t0, paired=FALSE) # p-value = 1 ?
