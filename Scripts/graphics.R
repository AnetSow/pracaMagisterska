# Libraries loading
library(plotrix)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(corrplot)


# Choose a directory for saving figures
dir <- setwd(choose.dir())


# Function automating the process of saving plots
# Arguments: name of a file (without extention) passed as a string
figure_saving <- function(fileName){
  filePath <- file.path(dir, paste("Fig_", fileName, ".png", sep = ""))
  dev.copy(png, filePath)
  dev.off()
}


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

figure_saving("piechart_1")


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
# pie3D(results_by_gr, labels = labels_by_gr, labelcex = 1.1, explode = 0.05, 
#       theta = 1, radius = 1, start = 1.5, col = c("#C2185B", "#C5E1A5", "#1B5E20"))
# legend("bottomleft", c("< 100 ppb VSC (normal)", "100 - 180 ppb VSC", "180 - 250 ppb VSC"), 
#        cex  = 0.8, fill = c("#C2185B", "#C5E1A5", "#1B5E20"))

pie3D(results_by_gr, labels = labels_by_gr, labelcex = 1.5, explode = 0.05, 
      theta = 0.9, radius = 1, start = 1.5, col = c("#d73027",  "#d9ef8b","#1a9850"))
legend("bottomleft", c("< 100 ppb VSC", "100 - 180 ppb VSC", "180 - 250 ppb VSC"),
       cex  = 1.3, fill = c("#d73027", "#1a9850", "#d9ef8b"), bty="n")

figure_saving("2.1a2")



# Histogram of the distribution of VSC mean concentration in exhaled air in patients before surgery, calculated on the basis of 3 measurements (Ia, Ib, Ic). Based on the average, we assess the severity of halitosis.


ggplot(VSC_before) +
  geom_bar(mapping = aes(x = Grupa, fill = Plec), position = "dodge", colour="black", width=0.5) + 
  scale_fill_manual(name = "Sex", values=c("#fee08b" ,"#1a9850"), labels = c("Female", "Male")) +
  scale_y_continuous("Size", breaks=c(0:20)) + 
  labs(x = "Group", y = "Size") +
  theme_classic() 


ggplot(VSC_before) +
  geom_bar(mapping = aes(x = Grupa, fill = Plec), position = "dodge", colour="black", width=0.5) + 
  scale_fill_manual(name = "Sex", values=c("#d73027" ,"skyblue2"), labels = c("Female", "Male")) +
  scale_y_continuous("Size", breaks=c(0:20)) + 
  labs(x = "Group", y = "Size") +
  theme_classic() 

figure_saving("2.1b2")



# Age distributions in groups plot

ggplot(data = VSC_before, mapping = aes(x = Grupa, y = Wiek, fill=Grupa)) +
  geom_boxplot() + 
  scale_fill_manual(name = "Group", values=c("#C2185B", "#C5E1A5", "#1B5E20")) +
  labs(title="Age distributions in groups", x = "Group", y = "Age") + 
  theme_classic()

figure_saving("age_per_groups")


ggplot(data = VSC_before, mapping = aes(x = Grupa, y = Wiek, fill=Plec)) +
  geom_boxplot() + 
  scale_fill_manual(name = "Sex", values=c("#CC6677", "#88CCEE"), labels = c("Female", "Male")) +
  labs(title="Age distributions in groups regarding to sex", x = "Group", y = "Age") +
  theme_classic() 

figure_saving("age_per_sex")



# Boxplots for VSC (before tonfillectomy)

ggplot(data = VSC_before, mapping = aes(x = Grupa, y = SredniaPomiar1, fill=Grupa)) +
  geom_boxplot() + 
  scale_fill_manual(name = "Group", values=c("#C2185B", "#C5E1A5", "#1B5E20")) +
  labs(x = "Group", y = "VSC [ppb]") +
  scale_y_continuous(n.breaks = 10) + 
  theme_classic() 

figure_saving("vcs_per_group")


ggplot(data = VSC_before, mapping = aes(x = Grupa, y = SredniaPomiar1, fill=Plec)) +
  geom_boxplot() + 
  scale_fill_manual(name = "Sex", values=c("#d73027", "#91cf60"), labels = c("Female", "Male")) +
  labs(x = "Group", y = "VSC [ppb]") +
  scale_y_continuous(n.breaks = 10) + 
  theme_classic() 

figure_saving("3.1a2")



# Boxplot for VSC (after tonfillectomy)

ggplot(data = VSC_after, mapping = aes(x = Grupa, y = SredniaPomiar2, fill=Plec)) +
  geom_boxplot() + 
  scale_fill_manual(name = "Sex", values=c("#d73027",  "#91cf60"), labels = c("Female", "Male")) +
  labs(x = "Group", y = "VSC [ppb]") +
  scale_y_continuous(n.breaks = 10) + 
  theme_classic() 

figure_saving("3.1b2")



# Halitomerty - before and after tonsillectomy
# Original

bef.orig <- vsc_data$VSC[vsc_data$nr==1]
aft.orig <- vsc_data$VSC[vsc_data$nr==2]

d.orig = data.frame(x = c(bef.orig,aft.orig), Group=rep(c("after \ntonsillectomy","before \ntonsillectomy"), c(length(bef.orig), length(aft.orig))))


p1 <- ggplot(d.orig, aes(x=x, fill=Group))+
  geom_histogram(colour="black", binwidth = 15, position="dodge") +
  # geom_density(colour="black", alpha=0.8) +
  scale_x_continuous(name = "VSC levels in the original samples [ppb]", n.breaks = 15) +
  scale_y_continuous(name = "Count", n.breaks = 10) + 
  theme_classic()

# Bootstrap
bef <- bootdif(vsc_data$VSC, g=vsc_data$nr)$a
aft <- bootdif(vsc_data$VSC, g=vsc_data$nr)$b

d = data.frame(x = c(bef,aft), Group=rep(c("after \ntonsillectomy","before \ntonsillectomy"), c(length(bef), length(aft))))

p2 <- ggplot(d, aes(x=x, fill=Group)) + 
  geom_histogram(colour="black", binwidth = 3) +
  # geom_density(colour="black", alpha=.8) +
  scale_x_continuous(name = "VSC levels in the bootstrap populations [ppb]", n.breaks = 15) +
  scale_y_continuous(name = "Count", n.breaks = 10) + 
  theme_classic()


grid.arrange(p1, p2, nrow=2)

figure_saving("3.5")


# before <- bootForContinuous(data$SredniaPomiar1)$boot.mean
# after <- bootForContinuous(data$SredniaPomiar2)$boot.mean
# boot <- rbind(cbind(before, rep(0, length(before))), cbind(after, rep(1, length(after))))
# colnames(boot) <- c("level", "status")
# 
# binwidth <- diff(range(before))/30
# p = ggplot(boot, aes(x=boot, fill=status, color=status)) +
#   geom_histogram(aes(y=..density..), binwidth=binwidth, color="black", fill="lightblue") +
#   geom_density(alpha=.8) +
#   theme_classic()
