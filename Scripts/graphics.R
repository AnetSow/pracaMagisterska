# Libraries loading
library(plotrix)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(corrplot)



# Piechart no. 1
# Correct results (<= 100 ppb VSC)
normal <- sum(VSC_before$Grupa == "A") # 14
normal_percent <- round(normal*100/n, 2) # 34.15 %

# Incorrect results - halitosis (> 100 ppb VSC)
above <-  n - normal # 27
above_percent <- round(above*100/n, 2) # 65.85 %

results_agg <- c(normal_percent, above_percent)
labels_agg <- c("34.15 %", "65.85 %")
pie3D(results_agg, labels = labels_agg, labelcex = 1.1, explode = 0.05, 
      theta = 1, radius = 1, col = c("#C2185B", "#1B5E20"), start = 1.5, 
      main = "Proportion of patiens with halitosis \n and without halitosis in the study group")
legend("bottomleft", c("< 100 ppb VSC (norma)", "> 100 ppb VSC"), cex = 0.8, inset = c(0.09, 0.07), fill = results_agg)

# dev.copy(png,'./Figures/piechart_1.png')
dev.off()



# Piechart no. 2
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


results_by_gr <- c(grA_percent, grB_percent, grC_percent)
labels_by_gr <- c("34.15 %", "58.54 %", "7.32 %")
pie3D(results_by_gr, labels = labels_by_gr, labelcex = 1.1, explode = 0.05, 
      theta = 1, radius = 1, start = 1.5, col = c("#C2185B", "#C5E1A5", "#1B5E20"))
legend("bottomleft", c("< 100 ppb VSC (normal)", "100 - 180 ppb VSC", "180 - 250 ppb VSC"), 
       cex  = 0.8, fill = c("#C2185B", "#C5E1A5", "#1B5E20"))

# dev.copy(png, './Figures/Fig_2.1a.png')
dev.off()



# Histogram of the distribution of VSC mean concentration in exhaled air in patients before surgery, calculated on the basis of 3 measurements (Ia, Ib, Ic). Based on the average, we assess the severity of halitosis.

ggplot(VSC_before) +
  geom_bar(mapping = aes(x = Grupa, fill = Plec), position = "dodge", colour="black", width=0.5) + 
  scale_fill_manual(name = "Sex", values=c("#CC6677", "#88CCEE"), labels = c("Female", "Male")) +
  scale_y_continuous("Size", breaks=c(0:20)) + 
  labs(x = "Group", y = "Size") +
  theme_classic() 

# dev.copy(png,'./Figures/Fig_2.1b.png')
dev.off()



# Age distributions in groups plot

ggplot(data = VSC_before, mapping = aes(x = Grupa, y = Wiek, fill=Grupa)) +
  geom_boxplot() + 
  scale_fill_manual(name = "Group", values=c("#C2185B", "#C5E1A5", "#1B5E20")) +
  labs(title="Age distributions in groups", x = "Group", y = "Age") + 
  theme_classic()

# dev.copy(png,'./Figures/age_distr_groups.png')
dev.off()

ggplot(data = VSC_before, mapping = aes(x = Grupa, y = Wiek, fill=Plec)) +
  geom_boxplot() + 
  scale_fill_manual(name = "Sex", values=c("#CC6677", "#88CCEE"), labels = c("Female", "Male")) +
  labs(title="Age distributions in groups regarding to sex", x = "Group", y = "Age") +
  theme_classic() 

# dev.copy(png,'./Figures/age_distr_sex.png')
dev.off()



# Boxplots for VSC (before tonfillectomy)

ggplot(data = VSC_before, mapping = aes(x = Grupa, y = SredniaPomiar1, fill=Grupa)) +
  geom_boxplot() + 
  scale_fill_manual(name = "Group", values=c("#C2185B", "#C5E1A5", "#1B5E20")) +
  labs(x = "Group", y = "VSC [ppb]") +
  scale_y_continuous(n.breaks = 10) + 
  theme_classic() 

# dev.copy(png,'./Figures/vcs_per_group.png')
dev.off()

ggplot(data = VSC_before, mapping = aes(x = Grupa, y = SredniaPomiar1, fill=Plec)) +
  geom_boxplot() + 
  scale_fill_manual(name = "Sex", values=c("#CC6677", "#88CCEE"), labels = c("Female", "Male")) +
  labs(x = "Group", y = "VSC [ppb]") +
  scale_y_continuous(n.breaks = 10) + 
  theme_classic() 

# dev.copy(png,'./Figures/Fig_3.1a.png')
dev.off()



# Boxplot for VSC (after tonfillectomy)

ggplot(data = VSC_after, mapping = aes(x = Grupa, y = SredniaPomiar2, fill=Plec)) +
  geom_boxplot() + 
  scale_fill_manual(name = "Sex", values=c("#CC6677", "#88CCEE"), labels = c("Female", "Male")) +
  labs(x = "Group", y = "VSC [ppb]") +
  scale_y_continuous(n.breaks = 10) + 
  theme_classic() 

# dev.copy(png,'./Figures/Fig_3.1b.png')
dev.off()











