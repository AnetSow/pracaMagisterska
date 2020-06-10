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

# Living place encoding
livingPlace <- numeric(nrow(data))

livingPlace[data[,'Wies'] == TRUE] <- 1
livingPlace[data[,'MiastoPowyzej50tys'] == TRUE] <- 2
livingPlace[data[,'MiastoPonizej50tys'] == TRUE] <- 3

data <- cbind(data, livingPlace)





# WITHOUT SPLIT ON GENDER

# BOOTSTRAP FOR CONTINUOUS DATA (age, VSC before, VSC after)

bootForContinuous <- function(my.sample, B = 1000){
    n = length(my.sample)
    boot.mean = numeric(B)
    boot.me = numeric(B)
    boot.sd = numeric(B)
    boot.min = numeric(B)
    boot.max = numeric(B)
    
    for (i in 1:B){
        boot.sample = sample(my.sample, n, replace = T)
        boot.mean[i] = mean(boot.sample)
        boot.me[i] = median(boot.sample)
        boot.sd[i] = sd(boot.sample)
        boot.min[i] = min(boot.sample)
        boot.max[i] = max(boot.sample)
    }

    mean.se = sd(boot.mean)
    mean.bias = abs(mean(boot.mean) - mean(my.sample))
    mean.left.ci = quantile(boot.mean, prob=0.025)
    mean.right.ci = quantile(boot.mean, prob=0.975)
    
    me.se = sd(boot.mean)
    me.bias = abs(mean(boot.me) - median(my.sample))
    me.left.ci = quantile(boot.me, prob=0.025)
    me.right.ci = quantile(boot.me, prob=0.975)
    
    sd.se = sd(boot.sd)
    sd.bias = abs(mean(boot.sd) - sd(my.sample))
    sd.left.ci = quantile(boot.sd, prob=0.025)
    sd.right.ci = quantile(boot.sd, prob=0.975)
    
    min.se = sd(boot.min)
    min.bias = abs(mean(boot.min) - min(my.sample))
    min.left.ci = quantile(boot.min, prob=0.025)
    min.right.ci = quantile(boot.min, prob=0.975)
    
    max.se = sd(boot.max)
    max.bias = abs(mean(boot.max) - max(my.sample))
    max.left.ci = quantile(boot.max, prob=0.025)
    max.right.ci = quantile(boot.max, prob=0.975)
    
    boot.results <- as.data.frame(matrix(c(
                            mean(boot.mean), mean(boot.me), mean(boot.sd), mean(boot.min), mean(boot.max),
                            mean.se, me.se, sd.se, min.se, max.se,
                            mean.bias, me.bias, sd.bias, min.se, max.se,
                            mean.left.ci, me.left.ci, sd.left.ci, min.left.ci, max.left.ci,
                            mean.right.ci, me.right.ci, sd.right.ci, min.right.ci, max.right.ci),
                            byrow = T, ncol = 5, nrow = 5))

    colnames(boot.results) <- c("mean", "median", "sd", "min", "max")
    rownames(boot.results) <- c("T1", "se", "bias", "leftCI", "rightCI")
    
    # binwidth=0
    # require(ggplot2)
    # if ( is.null(binwidth) )
    #     binwidth = diff(range(boot.mean))/30
    # p = ggplot(data.frame(x=boot.mean),aes(x=x)) +
    #     geom_histogram(aes(y=..density..),binwidth=binwidth) +
    #     geom_density(color="red")
    # 
    # plot(p)
    
    return(boot.results)
}

# H0: the patients are equally distributed across Education, Profession and Living place.
# H1: the patients are not equally distributed across Education, Profession and Living place.

# Age
bootForContinuous(data$Wiek)

# Halitomerty - before and after tonsillectomy
bootForContinuous(data$SredniaPomiar1)
bootForContinuous(data$SredniaPomiar2)



# BOOTSTRAP FOR CATEGORICAL DATA (livingPlace, Education, Profession)

bootForCategorical <- function(my.sample, B = 1000){
    
    n = length(my.sample)
    boot.statistics = numeric(B)
    
    for (i in 1:B){
        boot.sample = sample(my.sample, n, replace = T)
        boot.statistics[i] = boot.sample
    }
    
    counts <- table(boot.statistics)
    stat <- chisq.test(counts)
    pval <- chisq.test(counts)$p.value
                    
    return(list( counts = counts, stat = stat, pval = pval))
}

# H0: the patients are equally distributed across Education, Profession and Living place.
# H1: the patients are not equally distributed across Education, Profession and Living place.

# Education
# 1 - elementary, 2 - secondary, 3 - higher education
bootForCategorical(data$Wyksztalcenie) # p-value 4.98676e-68

# Profession
# 1 - blue-collar worker, 2 - white-collar worker, 3 - student, 4- school-age student
bootForCategorical(data$KategoriaZawodowa) # p-value 4.070697e-107


# Living place 
# 1 - city, 2 - town, 3 - village
bootForCategorical(data$livingPlace) # p-value 1.097715e-226

# We can reject the null hypothesis that the patients are equally distributed across Education, Profession and Living place.




# WITH SPLIT ON GENDER

female.orig <- filter(data, Plec=='K')
male.orig <- filter(data, Plec=='M')


# BOOTSTRAP FOR CONTINUOUS DATA (age, VSC before, VSC after)

boot2groups <- function(data, feature, B = 100){
    n = with(data, by(feature, Plec, length))
    n
    
    female.samples = with(data, matrix(sample(feature[Plec == "K"], size = n[1]*B, replace = TRUE), B, n[1]))
    male.samples = with(data, matrix(sample(feature[Plec == "M"], size = n[2]*B, replace = TRUE), B, n[2]))
    
    s1 <- shapiro.test(female.samples)$p.value
    s2 <- shapiro.test(male.samples)$p.value
    var <- var.test(female.samples, male.samples)
    
    if( s1 < 0.05 || s2 < 0.05 || var < 0.05) {
        t <- wilcox.test(female.samples, male.samples, paired = FALSE)
        pval <- t$p.value
    }else{
        t <- t.test(female.samples, male.samples, paired = FALSE)
        pval <- t$p.value
    }
    
    return(list(female.samples = female.samples, male.samples = male.samples, s1 = s1, s2 = s2, var = var, t = t, pval = pval))
}

# Age
bootForContinuous(female.orig$Wiek)
bootForContinuous(male.orig$Wiek)
boot2groups(data, data$Wiek)  # p-value 5.39013e-28
# As the p-value turns out to be 5.39013e-28, and is less than the .05 significance level, we reject the null hypothesis that the age of female and male are identical populations.

# Halitometry - before and after tonsillectomy
bootForContinuous(female.orig$SredniaPomiar1)
bootForContinuous(male.orig$SredniaPomiar1)
boot2groups(data, data$SredniaPomiar1) # p-value 3.883904e-11

bootForContinuous(female.orig$SredniaPomiar2)
bootForContinuous(male.orig$SredniaPomiar2)
boot2groups(data, data$SredniaPomiar2) # p-value 0.002480751
# At .05 significance level, we conclude that the measuremenths of VSC levels of female and male, either before and after tonsillectomy, are nonidentical populations. 

# BOOTSTRAP FOR CATEGORICAL DATA (livingPlace, Education, Profession)

# Education
# 1 - elementary, 2 - secondary, 3 - higher education
bootForCategorical(female.orig$Wyksztalcenie) # p-value 2.855976e-93
bootForCategorical(male.orig$Wyksztalcenie) # p-value 1.305776e-27

# Profession
# 1 - blue-collar worker, 2 - white-collar worker, 3 - student, 4- school-age student
bootForCategorical(female.orig$KategoriaZawodowa) # p-value 7.045396e-173
bootForCategorical(male.orig$KategoriaZawodowa) # p-value 1.0444e-61

# Living place 
# 1 - village, 2 - city, 3 - town
bootForCategorical(female.orig$livingPlace) # p-value 1.077331e-182
bootForCategorical(male.orig$livingPlace) # p-value 9.721487e-155

# We can reject the null hypothesis that the patients are equally distributed across Education, Profession and Living place.



# Comparing VSC before and after

vsc_data <- rbind(cbind(data$SredniaPomiar1, rep(1,length(data$SredniaPomiar1))), cbind(data$SredniaPomiar2, rep(2,length(data$SredniaPomiar2)))) 
colnames(vsc_data) <- c("VSC", "nr")
vsc_data <- as.data.frame(vsc_data)
vsc_data

n = with(vsc_data, by(VSC, nr, length)); n

B = 1000
before.samples = with(vsc_data, matrix(sample(VSC[nr == 1], size = n[1]*B, replace = TRUE), B, n[1]))
after.samples = with(vsc_data, matrix(sample(VSC[nr == 2], size = n[2]*B, replace = TRUE), B, n[2]))

before.means = apply(before.samples, 1, mean)
after.means = apply(after.samples, 1, mean)

s1 <- shapiro.test(before.means)$p.value
s2 <- shapiro.test(after.means)$p.value
var <- var.test(before.means, after.means)

if( s1 < 0.05 || s2 < 0.05 || var < 0.05) {
    t <- wilcox.test(before.means, after.means, paired = TRUE)
    pval <- t$p.value
}else{
    t <- t.test(before.means, after.means, paired = FALSE)
    pval <- t$p.value
}

boot.stat = after.means - before.means


# OR
# http://biostat.mc.vanderbilt.edu/wiki/pub/Main/JenniferThompson/ms_mtg_18oct07.pdf


library(Hmisc)

bootdif <- function(y, g) {
    ## Ensure treatment group is a factor
    g <- as.factor(g)
    
    ## Use the smean.cl.boot function to bootstrap means for variable y for each treatment group (A and B)
    a <- attr(smean.cl.boot(y[g==levels(g)[1]],B=1000, reps=TRUE),'reps')
    b <- attr(smean.cl.boot(y[g==levels(g)[2]],B=1000, reps=TRUE),'reps')
    
    ## Calculate the observed mean difference between groups
    meandif <- diff(tapply(y, g, mean, na.rm=TRUE))
    ## Calculate the 2.5 and 97.5 percentiles of the differences in bootstrapped means (can easily be changed for 90% CI, 99% CI, etc.)
    a.b <- quantile(b-a, c(.025,.975))
    ## Prepare object to return
    res <- c(meandif, a.b)
    names(res) <- c('Mean','.025','.975')
    # res
    return(list(a=a,b=b,res=res))
    }
bootdif(vsc_data$VSC, g=vsc_data$nr)





bef <- bootdif(vsc_data$VSC, g=vsc_data$nr)$a
aft <- bootdif(vsc_data$VSC, g=vsc_data$nr)$b

d = data.frame(x = c(bef,aft), Group=rep(c("after \ntonsillectomy","before \ntonsillectomy"), c(length(bef), length(aft))))

ggplot(d) + 
    geom_histogram(aes(x=x, fill=Group), colour="black", binwidth = 3) +
    scale_x_continuous(name = "VSC levels in bootstrap populations [ppb]", n.breaks = 10) +
    scale_y_continuous(name = "Count", n.breaks = 15) + 
    theme_classic()

figure_saving("3.4.2")

# "#d73027", "#91cf60"

