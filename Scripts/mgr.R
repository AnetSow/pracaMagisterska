# Working directory setting
# setwd("C:/Users/Aneta/Documents/Magisterka/Scripts")

# Libraries loading
library(tidyverse)
library(dplyr)
library(plotrix)
library(ggplot2)
library(moments)
library(factoextra)
library(FactoMineR)
library(corrplot)


# Data loading
data <- read.csv(file="C:/Users/Aneta/Documents/Magisterka/data/dane_mgr.csv", header=TRUE, sep=";")

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

n = nrow(data) # liczebnoœæ pacjentów = 41




## EXPLORATORY DATA ANALYSIS (BASIC STATISTICS DESCRIBING GROUP OF PATIENTS) ##

# WITHOUT SPLIT ON GENDER

data %>%
  summarise( n=n(),
             AvgAge = mean(Wiek, na.rm=T),
             Village = sum(!is.na(Wies)/n*100), 
             TownBelow_50k = sum(!is.na(MiastoPonizej50tys))/n*100,
             TownAbove_50k = sum(!is.na(MiastoPowyzej50tys)))/n*100

summary(data$Wiek) # summary for age

wiek_m <- data$Wiek[data$Plec=="M"]
wiek_k <- data$Wiek[data$Plec=="K"]
t.test(wiek_m, wiek_k, paired=FALSE)


data %>% 
  count(KategoriaZawodowa) %>% 
  mutate(procent = n/sum(n)*100)

data %>% 
  count(Wyksztalcenie) %>% 
  mutate(procent = n/sum(n)*100)


# WITH SPLIT ON GENDER

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

job <- data %>% 
  group_by(Plec) %>% 
  count(KategoriaZawodowa) %>% 
  mutate(procent = n/sum(n)*100)


education <- data %>% 
  group_by(Plec) %>% 
  count(Wyksztalcenie) %>% 
  mutate(procent = n/sum(n)*100)

livingPlace_matrix <- matrix(rbind(c(5,2,22), c(0,1,11)),nrow=2,ncol=3)
chisq.test(livingPlace_matrix) # p-value = 0.3078

job_matrix <- matrix(rbind(c(filter(job, Plec == "K")$n), c(filter(job, Plec == "M")$n)),nrow=2,ncol=4)
chisq.test(job_matrix) # p-value = 0.1834

edu_matrix <- matrix(rbind(c(filter(education, Plec == "K")$n), c(filter(education, Plec == "M")$n)),nrow=2,ncol=4)
chisq.test(edu_matrix) # p-value = 0.2699
## No statistically significant difference was observed in terms of analyzed features (p > 0.05).




## STATISTICS FOR VSC (volatile sulphur compounds, lotne zwi¹zki siarki)

VSC_before <- select(data, Plec, Wiek, SredniaPomiar1)
summary(VSC_before)


VSC_before_stats <- VSC_before %>%
  group_by(Plec) %>% 
  summarise(n = n(), 
            AvgAge = mean(Wiek, na.rm=T), 
            AvgVSC = mean(SredniaPomiar1, na.rm=T), 
            Sd = sd(SredniaPomiar1), 
            Median = median(SredniaPomiar1), 
            Min = min(SredniaPomiar1), 
            Max = max(SredniaPomiar1))
as.data.frame(VSC_before_stats)

VSC_before_f <- filter(VSC_before, Plec=="K")$SredniaPomiar1
vsc_before_m <- filter(VSC_before, Plec=="M")$SredniaPomiar1

shapiro.test(VSC_before_f) #  p-value = 0.2542
shapiro.test(vsc_before_m) # p-value = 0.3716

var.test(VSC_before_f, vsc_before_m) # p-value = 0.01254, ratio of variances 0.313
# samples of VSC measurements for both sexes are normally distributed , however the assumption of homogeneity of variance is not satisfied --> non-parametric test

wilcox.test(VSC_before_f, vsc_before_m) # p-value = 0.5378 --> no difference between sample means



VSC_after_f <- filter(VSC_after, Plec=="K")$SredniaPomiar2
vsc_after_m <- filter(VSC_after, Plec=="M")$SredniaPomiar2

shapiro.test(VSC_after_f) #  p-value = 0.3131
shapiro.test(vsc_after_m) # p-value = 0.005966 < 0.05
# samples of VSC measurements for male are not normally distributed --> non-parametric test

wilcox.test(VSC_after_f, vsc_after_m, paired=FALSE) #  p-value = 0.8185 --> no difference between sample means



## Division of the group into research groups A - D according to the results of measurements of VSC concentration in exhaled air (before performing the operation) and the literature thresholds.

VSC_before <- mutate(VSC_before, Grupa = SredniaPomiar1)

VSC_before$Grupa[VSC_before$SredniaPomiar1 <= 100] = "A"
VSC_before$Grupa[VSC_before$SredniaPomiar1 > 100 & VSC_before$SredniaPomiar1 <= 180] = "B"
VSC_before$Grupa[VSC_before$SredniaPomiar1 > 180 & VSC_before$SredniaPomiar1 <= 250] = "C"
VSC_before$Grupa[VSC_before$SredniaPomiar1 > 250] = "D"


          ## AGGREGATED ##
# CORRECT RESULTS (<= 100 ppb VSC)
normal <- sum(VSC_before$Grupa == "A") # 14
normal_percent <- round(normal*100/n, 2) # 34.15 %

# INCORRECT RESULTS - halitosis (> 100 ppb VSC)
above <-  n - normal # 27
above_percent <- round(above*100/n, 2) # 65.85 %


# Piechart no. 1
results_agg <- c(normal_percent, above_percent)
labels_agg <- c("34.15 %", "65.85 %")
pie3D(results_agg, labels = labels_agg, labelcex = 1.1, explode = 0.05, 
      theta = 1, radius = 1, col = c("#C2185B", "#1B5E20"), start = 1.5, 
      main = "Proportion of patiens with halitosis \n and without halitosis in the study group")
legend("bottomleft", c("< 100 ppb VSC (norma)", "> 100 ppb VSC"), cex = 0.8, inset = c(0.09, 0.07), fill = results_agg)

# dev.copy(png,'C:/Users/Aneta/Documents/Magisterka/Figures/piechart_1.png')
dev.off()


          ## BY GROUPS ##
# Group A - normal (< 100 ppb VSC)
grA <- sum(VSC_before$Grupa == "A") # 14
grA_percent <- round(grA*100/n, 2) # 34.15 %

# Group B - light form of halitosis (100 - 180 ppb VSC)
grB <- sum(VSC_before$Grupa == "B") # 24
grB_percent <- round(grB*100/n, 2) # 58.54 %

# Group C - moderate form of halitosis (180 - 250 ppb VSC)
grC <- sum(VSC_before$Grupa == "C") # 3
grC_percent <- round(grC*100/n, 2) # 7.32 %

# Group D - severe form of halitosis (> 250 ppb VSC)
grD <- sum(VSC_before$Grupa == "D") # 0
grD_percent <- grD*100/n # 0 %


# Piechart no. 2
results_by_gr <- c(grA_percent, grB_percent, grC_percent)
labels_by_gr <- c("34.15 %", "58.54 %", "7.32 %")
pie3D(results_by_gr, labels = labels_by_gr, labelcex = 1.1, explode = 0.05, 
      theta = 1, radius = 1, start = 1.5, col = c("#C2185B", "#C5E1A5", "#1B5E20"))
legend("bottomleft", c("< 100 ppb VSC (normal)", "100 - 180 ppb VSC", "180 - 250 ppb VSC"), 
       cex = 0.8, fill = c("#C2185B", "#C5E1A5", "#1B5E20"))

# dev.copy(png,'C:/Users/Aneta/Documents/Magisterka/Figures/Fig_2.1a.png')
dev.off()



# Histogram of the distribution of VSC mean concentration in exhaled air in patients before surgery, calculated on the basis of 3 measurements (Ia, Ib, Ic). Based on the average, we assess the severity of halitosis.

ggplot(VSC_before) +
  geom_bar(mapping = aes(x = Grupa, fill = Plec), position = "dodge", colour="black", width=0.5) + 
  scale_fill_manual(name = "Sex", values=c("#CC6677", "#88CCEE"), labels = c("Female", "Male")) +
  scale_y_continuous("Size", breaks=c(0:20)) + 
  labs(x = "Group", y = "Size") +
  theme_classic() 

# dev.copy(png,'./Figures/Fig_2.1b.png')
# png('C:/Users/Aneta/Documents/Magisterka/Figures/Fig_2.1b.png')
dev.off()




#  The chi-square test of independence (2x3, 2 degrees of freedom)
contingency.table <- table(VSC_before$Grupa, VSC_before$Plec)
        #    K  M
        # A  9  5
        # B 19  5
        # C  1  2
observed <- t(matrix(c(9, 19,  1, 5,  5, 2),nrow=3,ncol=2))
chisq.test(observed)
# Pearson's Chi-squared test
# 
# data:  observed
# X-squared = 3.1326, df = 2, p-value = 0.2088 

# No statistically significant difference was observed in terms of analyzed features (p > 0.05).



# Distributions and homogenity check for Age ang Sex in groups.

shapiro.test(filter(VSC_before, Grupa == "A")$Wiek) # p-value = 0.4122
shapiro.test(filter(VSC_before, Grupa == "A", Plec == "K")$Wiek) # p-value = 0.8672
shapiro.test(filter(VSC_before, Grupa == "A", Plec == "M")$Wiek) # p-value = 0.05023

shapiro.test(filter(VSC_before, Grupa == "B") $Wiek) # p-value = 7.823e-05 < 0.05
shapiro.test(filter(VSC_before, Grupa == "B", Plec == "K")$Wiek) # p-value = 0.0002648
shapiro.test(filter(VSC_before, Grupa == "B", Plec == "M")$Wiek) # p-value = 0.2866

shapiro.test(filter(VSC_before, Grupa == "C") $Wiek) # p-value = 0.253
shapiro.test(filter(VSC_before, Grupa == "C", Plec == "K")$Wiek) # n<3
shapiro.test(filter(VSC_before, Grupa == "C", Plec == "M")$Wiek) # n<3

# "Age" variable is normally distributed in A and C group (when testing both genders) and in B group (when testing males).
# The distribution is not normal among women in group B and generally in B group. The reason for that are probably two outliers (see the plot below).

ggplot(data = VSC_before, mapping = aes(x = Grupa, y = Wiek, fill=Grupa)) +
  geom_boxplot() + 
  scale_fill_manual(name = "Group", values=c("#C2185B", "#C5E1A5", "#1B5E20")) +
  labs(title="Age distributions in groups", x = "Group", y = "Age") + 
  theme_classic()

ggplot(data = VSC_before, mapping = aes(x = Grupa, y = Wiek, fill=Plec)) +
  geom_boxplot() + 
  scale_fill_manual(name = "Sex", values=c("#CC6677", "#88CCEE"), labels = c("Female", "Male")) +
  labs(title="Age distributions in groups regarding to sex", x = "Group", y = "Age") +
  theme_classic() 


bartlett.test(VSC_before$Wiek, g=VSC_before$Grupa)
# Bartlett test of homogeneity of variances
# 
# data:  VSC_before$Wiek and VSC_before$Grupa
# Bartlett's K-squared = 4.3089, df = 2, p-value = 0.116

# The variances in each of the groups are the same.




## Analysis of VSC measurements BEFORE tonsillectomy in individual groups. ##

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
# The average level of VSC is the highest in group C - it is 326.08% higher than in Group A and 155.02% higher than in Group B.

ggplot(data = VSC_before, mapping = aes(x = Grupa, y = SredniaPomiar1, fill=Grupa)) +
  geom_boxplot() + 
  scale_fill_manual(name = "Group", values=c("#C2185B", "#C5E1A5", "#1B5E20")) +
  labs(x = "Group", y = "VSC [ppb]") +
  scale_y_continuous(n.breaks = 10) + 
  theme_classic() 

ggplot(data = VSC_before, mapping = aes(x = Grupa, y = SredniaPomiar1, fill=Plec)) +
  geom_boxplot() + 
  scale_fill_manual(name = "Sex", values=c("#CC6677", "#88CCEE"), labels = c("Female", "Male")) +
  labs(x = "Group", y = "VSC [ppb]") +
  scale_y_continuous(n.breaks = 10) + 
  theme_classic() 


shapiro.test(VSC_after$SredniaPomiar2[VSC_after$Plec=="K"]) # p-value = 0.2542
shapiro.test(VSC_after$SredniaPomiar2[VSC_after$Plec=="M"]) # p-value = 0.3716
bartlett.test(VSC_after$SredniaPomiar2, g=VSC_before$Plec) #  p-value = 0.01564
t.test(VSC_after$SredniaPomiar2[VSC_after$Plec=="M"], VSC_after$SredniaPomiar2[VSC_after$Plec=="K"], paired=FALSE)



## Analysis of VSC measurements AFTER tonsillectomy in individual groups. ##


VSC_after <- select(data, Plec, Wiek, SredniaPomiar2)
summary(VSC_after)

VSC_after$Grupa[VSC_after$SredniaPomiar2 <= 100] = "A"
VSC_after$Grupa[VSC_after$SredniaPomiar2 > 100 & VSC_after$SredniaPomiar2 <= 180] = "B"
VSC_after$Grupa[VSC_after$SredniaPomiar2 > 180 & VSC_after$SredniaPomiar2 <= 250] = "C"
VSC_after$Grupa[VSC_after$SredniaPomiar2 > 250] = "D"

# CORRECT RESULTS (<= 100 ppb VSC)
normal_after <- sum(VSC_after$Grupa == "A") # 40
normal_after_percent <- round(normal_after*100/n, 2) # 97.56 %

# INCORRECT RESULTS - halitosis (> 100 ppb VSC)
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


shapiro.test(data$SredniaPomiar1) # p-value = 0.5822
shapiro.test(data$SredniaPomiar2) # p-value = 0.04493
# The average VSC measurements taken before surgery have a normal distribution, and after the operation deviate from it.

hist(data$SredniaPomiar2)
skewness(data$SredniaPomiar2) # 0.7873434, right-skewed distribution --> Box-Cox transformation
skewness(sqrt(data$SredniaPomiar2)) # 0.01777133
shapiro.test(sqrt(data$SredniaPomiar2)) # p-value = 0.5834 --> ok

data$SredniaPomiar2norm <- sqrt(data$SredniaPomiar2)

t.test(data$SredniaPomiar1, data$SredniaPomiar2norm, paired=T) # p-value < 2.2e-16
# We have basis for rejecting the null hypothesis that the means of two samples of dependent distributions do not differ from each other. The difference is 106.5 ppb.


ggplot(data = VSC_after, mapping = aes(x = Grupa, y = SredniaPomiar2, fill=Plec)) +
  geom_boxplot() + 
  scale_fill_manual(name = "Sex", values=c("#CC6677", "#88CCEE"), labels = c("Female", "Male")) +
  labs(x = "Group", y = "VSC [ppb]") +
  scale_y_continuous(n.breaks = 10) + 
  theme_classic() 





## PRINCIPAL COMPONENT ANALYSIS ##

# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/


# Selecting numerical variables for PCA
nums <- unlist(lapply(data, is.integer)) 
dataPCA <- data[ , nums]
colnames(dataPCA)
dataPCA <- dataPCA[ , !names(dataPCA) %in% c("SkalaRosenberga")]
summary(dataPCA) # 41 individuals, 11 variables


# Obliczenie PCA
pca <- PCA(dataPCA, graph = FALSE) # data are standarized by default
print(pca)

eig.val <- pca$eig
eig.val

# wartoœci w³asne (eigenvalues) sk³adowych g³ównych PC1-PC3  > 1.0 --> za punkt odciêcia mo¿na przyj¹æ PC3, gdy¿ 82.6% ca³kowitej warinacji jest wyjaœniana przez pierwsze 3 sk³adowe g³ówne

# Alternatywn¹ metod¹ oszacowania liczby sk³adowych g³ównych jest wykres osypiska, który przedstawia wartoœci w³asne uporz¹dkowane malej¹co. Liczba sk³adowych g³ównych wybierana jest w punkcie, w którym nastêpuje relatywny spadek iloœci wyjaœnianej wariancji przez dan¹ sk³adow¹ (Jollife 2002, Peres-Neto, Jackson, and Somers (2005)).

fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50)) 

# RESULTS FOR VARIABLES
var <- get_pca_var(pca)
var

head(var$coord)

# wykres korelacji zmiennych - pokazuje relacje pomiêdzy wszystkimi zmiennymi
fviz_pca_var(pca, col.var="steelblue", repel=TRUE)

head(var$cos2) # Cos2 (cosinus kwadratowy, wspó³rzêdne kwadratowe): Wartoœci cos2 s³u¿¹ do oszacowania jakoœci reprezentacji zmiennych na mapie czynników.

# corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(pca, choice = "var", axes = 1:2)

fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("#C2185B", "#C5E1A5", "#1B5E20"), 
             repel = TRUE) 
# Z wykresów jednoznacznie wynika, ¿e zmienne Waga, Wzrost i Wiek nie wp³ywaj¹ na przydzia³ obserwacji do grupy.

head(var$contrib) # Contributions to the principal components

# corrplot(var$contrib, is.corr=FALSE) # aby wyró¿niæ najbardziej przyczyniaj¹ce siê do wariancji zmienne dla ka¿dego wymiaru 

fviz_contrib(pca, choice = "var", axes = 1) # Contributions of variables to PC1
fviz_contrib(pca, choice = "var", axes = 2) # Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 1:2) # Total

# Czerwona przerywana linia oznacza spodziewany uœredniony udzia³ zmiennej w wyjaœnieniu wariancji. Dla zestandaryzowanych danych = 1/length(variables) = 1/12 = 8,33%. Dla poszczególnych komponentów, zmienna z udzia³em przewy¿szaj¹cym tê wartoœæ mo¿e byæ rozwa¿ana jako znacz¹ca.

# Mo¿na zauwa¿yæ, ¿e zmienne Waga, Wzrost i Wiek uczestnicz¹ najsilniej w wyjaœnieniu zmiennoœci dla wymiaru 1 i 2.

fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("#C2185B", "#C5E1A5", "#1B5E20"))


res.desc <- dimdesc(pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1
res.desc$Dim.2 # wszêdzie p-value < 0.05 zatem odrzucamy H0, ¿e prawdziwy wspó³czynnik korelacji = 0
pca$var

# RESULTS FOR OBSERVATIONS (individuals)
ind <- get_pca_ind(pca)
ind

# Coordinates of individuals
head(ind$coord)
# Quality of individuals
head(ind$cos2)
# Contributions of individuals
ind$contrib

fviz_pca_ind(pca, col.ind = "cos2", pointsize = 2,
             gradient.cols = c("#C2185B", "#C5E1A5", "#1B5E20")) # Obserwacje podobne do siebie tworz¹ zgrupowanie na wykresie.

fviz_cos2(pca, choice = "ind", axes = 1:2)

# dataPCA <- mutate(dataPCA, Grupa = SredniaPomiar1)
# dataPCA$Grupa[dataPCA$SredniaPomiar1 <= 100] = "A"
# dataPCA$Grupa[dataPCA$SredniaPomiar1 > 100 & dataPCA$SredniaPomiar1 <= 180] = "B"
# dataPCA$Grupa[dataPCA$SredniaPomiar1 > 180 & dataPCA$SredniaPomiar1 <= 250] = "C"
# dataPCA$Grupa[dataPCA$SredniaPomiar1 > 250] = "D"

fviz_pca_ind(pca,
             geom.ind = "point", # show points only (but not "text")
             col.ind = VSC_before$Grupa, # color by groups
             palette = c("#C2185B", "#C5E1A5", "#1B5E20"),
             addEllipses = TRUE, 
             # ellipse.type = "confidence", # concentration/confidence/convex ellipses
             legend.title = "Groups")


fviz_pca_biplot(pca, 
                geom.ind = "point", 
                geom.var = c("arrow", "text"), 
                col.ind = VSC_before$Grupa,
                fill.ind = "white", 
                col.var = "steelblue", 
                fill.var = "brack",
                gradient.cols = NULL, 
                label = "all", 
                palette = c("#C2185B", "#C5E1A7", "#1B5E20"),
                addEllipses = FALSE,
                legend.title = "Groups",
                title = " ")



# # Wykres rozk³adu pochodzenia (miasto powy¿ej/poni¿ej 50tys, wieœ) z rozró¿nieniem na p³eæ.
# 
# data$Pochodzenie <- 0
# data$Pochodzenie[data$MiastoPowyzej50tys == "TRUE"] = 3
# data$Pochodzenie[data$MiastoPonizej50tys == "TRUE"] = 2
# data$Pochodzenie[data$Wies == "TRUE"] = 1
# 
# shapiro.test(data$Pochodzenie) # p-value = 1.176e-10 --> brak rozk³adu normalnego
# kruskal.test(data$SredniaPomiar1,g=data$Pochodzenie) # p-value = 0.1523 --> brak ró¿nic pomiêdzy grupami
# kruskal.test(data$SredniaPomiar2,g=data$Pochodzenie) # p-value = 0.06132 --> obecne ró¿nice pomiêdzy grupami
# 
# library(pgirmess)
# kruskalmc(data$SredniaPomiar2,data$Pochodzenie) # brak istotnych statystycznie ró¿nic
# 
# Pochodzenie <- data %>%
#   group_by(Pochodzenie) %>% 
#   summarise(Liczebnosc = n(),
#             PoziomVSC_przed = round(mean(SredniaPomiar1, na.rm=T), 2),
#             PoziomVSC_po = round(mean(SredniaPomiar2, na.rm=T), 2))
# 
# Pochodzenie <- as.data.frame(Pochodzenie)[,2:4]




# # SkalaRosenberga - ocenia intensywnoœæ zapachu
# 
# Rosenberg <- data %>%
#   group_by(SkalaRosenberga) %>% 
#   summarise(PoziomVSC_przed = round(mean(SredniaPomiar1, na.rm=T), 2),
#             Liczebnosc = n(),
#             PoziomVSC_po = round(mean(SredniaPomiar2, na.rm=T), 2))
# 
# Rosenberg <- as.data.frame(Rosenberg)[,2:4]
# 
# par(mfrow=c(1,1), 
#         col = "steelblue", 
#         main = "Stê¿enie VSC przed i po operacji \n w grupach wyró¿nionych na podstawie badania \n intensywnoœci halitozy wg. skali Rosenberga.",
#         xlab = "Skala Rosenberga", ylab = "Stê¿enie VSC [ppb]")
# barplot(Rosenberg$PoziomVSC_przed, names.arg = c(0:5))
# barplot(Rosenberg$PoziomVSC_po, names.arg = c(0:5))


# ## MICROBIOLOGICAL RESEARCH ##
# 
# # A function returns all bacterial strain names in a given column
# get_bacterial_strains = function(x) {
#   strains <- x[x != "ujemny"]
#   strains <- paste(strains, collapse = ", ")
#   strains <- unlist(strsplit(strains, ","))
#   strains <- trimws(strains)
#   return(strains)
# }
# 
# # for both groups
# anaerobes_both <- get_bacterial_strains(data$WymazMigdalkiBeztlenowe)
# aerobic_both <- get_bacterial_strains(data$WymazMigdalkiTlenowe)
# 
# length(unique(anaerobes_both))
# length(unique(aerobic_both))
# 
# 
# # dla grupy kontrolnej PRZED OPERACJ¥
# 
# wymazMigd <- data[, c('SredniaPomiar1', 'SredniaPomiar2', 'WymazMigdalkiBeztlenowe','WymazMigdalkiTlenowe')]
# wymazMigd_Kontrola <- wymazMigd[ wymazMigd$SredniaPomiar1 <= 100, ]
# 
# beztlenoweMigd_Kontrola <- get_bacterial_strains(wymazMigd_Kontrola$WymazMigdalkiBeztlenowe)
# tlenoweMigd_Kontrola <- get_bacterial_strains(wymazMigd_Kontrola$WymazMigdalkiTlenowe)
# 
# length(beztlenoweMigd_Kontrola) # 8
# length(tlenoweMigd_Kontrola) # 39
# 
# 
# # dla grupy badawczej PRZED OPERACJ¥
# 
# wymazMigd_Halitoza <- wymazMigd[ wymazMigd$SredniaPomiar1 > 100, ]
# 
# beztlenoweMigd_Halitoza <- get_bacterial_strains(wymazMigd_Halitoza$WymazMigdalkiBeztlenowe)
# tlenoweMigd_Halitoza <- get_bacterial_strains(wymazMigd_Halitoza$WymazMigdalkiTlenowe)
# 
# length(beztlenoweMigd_Halitoza) # 2
# length(tlenoweMigd_Halitoza) # 27
# 
# 
# # dla grupy kontrolnej PO OPERACJI
# 
# wymazMigd_Kontrola2 <- wymazMigd[ wymazMigd$SredniaPomiar2 <= 100, ]
# 
# beztlenoweMigd_Kontrola2 <- get_bacterial_strains(wymazMigd_Kontrola2$WymazMigdalkiBeztlenowe)
# tlenoweMigd_Kontrola2 <- get_bacterial_strains(wymazMigd_Kontrola2$WymazMigdalkiTlenowe)
# 
# length(beztlenoweMigd_Kontrola2) # 0
# length(tlenoweMigd_Kontrola2) # 3
# 
# # dla grupy badawczej PO OPERACJI
# wymazMigd_Halitoza2 <- wymazMigd[ wymazMigd$SredniaPomiar2 > 100, ]
# 
# beztlenoweMigd_Halitoza2 <- get_bacterial_strains(wymazMigd_Halitoza2$WymazMigdalkiBeztlenowe)
# tlenoweMigd_Halitoza2 <- get_bacterial_strains(wymazMigd_Halitoza2$WymazMigdalkiTlenowe)
# 
# length(beztlenoweMigd_Halitoza2) # 10
# length(tlenoweMigd_Halitoza2) # 63
# 
# 
# # Tabela przedstawiaj¹ca liczbê ró¿nych szczepów bakterii/grzybów 
# 
# data.frame("Grupa"=c("beztlenowe","tlenowe"), 
#            "Norma_PRZED"=c(length(beztlenoweMigd_Kontrola), length(tlenoweMigd_Kontrola)),
#            "Halitoza_PRZED"=c(length(beztlenoweMigd_Halitoza), length(tlenoweMigd_Halitoza)), 
#            "Norma_PO"=c(length(beztlenoweMigd_Kontrola2), length(tlenoweMigd_Kontrola2)),
#            "Halitoza_PO"=c(length(beztlenoweMigd_Halitoza2), length(tlenoweMigd_Halitoza2)),
#            "Razem"=c(length(anaerobes_both), length(aerobic_both)))

