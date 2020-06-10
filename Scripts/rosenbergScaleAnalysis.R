## SkalaRosenberga - ocenia intensywnosc zapachu

Rosenberg <- data %>%
  group_by(SkalaRosenberga) %>%
  summarise(VSC_before = round(mean(SredniaPomiar1, na.rm=T), 2),
            Liczebnosc = n(),
            VSC_after = round(mean(SredniaPomiar2, na.rm=T), 2))


scales <- rbind(Rosenberg$VSC_before,Rosenberg$VSC_after)

barplot(scales, beside=T, col=c("#FFCC66","#3366CC"), 
        main="Levels of VSC in Rosenberg scale")

legend("topleft", inset=0.05, c("before tonsillectomy", "after tonsillectomy"), 
       fill=c("#FFCC66","#3366CC"), cex=0.8, box.lty=0)



# Correlations between organoleptic and halimetric study

M <- cbind(data$SkalaRosenberga,data$SredniaPomiar1,data$SredniaPomiar2)
colnames(M) <- c("RSES", "pre", "post")
cor(M)


library(PerformanceAnalytics)

chart.Correlation(M, method="pearson")
# On the diagonal are the univariate distributions, plotted as histograms and kernel density plots. On the right of the diagonal are the pair-wise correlations, with red stars signifying significance levels. As the correlations get bigger the font size of the coefficient gets bigger. On the left side of the diagonal is the scatter-plot matrix, with loess smoothers in red to help illustrate the underlying relationship. 
