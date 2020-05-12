## SkalaRosenberga - ocenia intensywno?? zapachu

Rosenberg <- data %>%
  group_by(SkalaRosenberga) %>%
  summarise(PoziomVSC_przed = round(mean(SredniaPomiar1, na.rm=T), 2),
            Liczebnosc = n(),
            PoziomVSC_po = round(mean(SredniaPomiar2, na.rm=T), 2))

Rosenberg <- as.data.frame(Rosenberg)[,2:4]

par(mfrow=c(1,1),
    col = "steelblue",
    main = "St??enie VSC przed i po operacji \n w grupach wyr??nionych na podstawie badania \n intensywno?ci halitozy wg. skali Rosenberga.",
    xlab = "Skala Rosenberga", ylab = "St??enie VSC [ppb]")
barplot(Rosenberg$PoziomVSC_przed, names.arg = c(0:5))
barplot(Rosenberg$PoziomVSC_po, names.arg = c(0:5))
