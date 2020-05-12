# Wykres rozk?adu pochodzenia (miasto powy?ej/poni?ej 50tys, wie?) z rozr??nieniem na p?e?.

data$Pochodzenie <- 0
data$Pochodzenie[data$MiastoPowyzej50tys == "TRUE"] = 3
data$Pochodzenie[data$MiastoPonizej50tys == "TRUE"] = 2
data$Pochodzenie[data$Wies == "TRUE"] = 1

shapiro.test(data$Pochodzenie) # p-value = 1.176e-10 --> brak rozk?adu normalnego
kruskal.test(data$SredniaPomiar1,g=data$Pochodzenie) # p-value = 0.1523 --> brak r??nic pomi?dzy grupami
kruskal.test(data$SredniaPomiar2,g=data$Pochodzenie) # p-value = 0.06132 --> obecne r??nice pomi?dzy grupami

library(pgirmess)
kruskalmc(data$SredniaPomiar2,data$Pochodzenie) # brak istotnych statystycznie r??nic

Pochodzenie <- data %>%
  group_by(Pochodzenie) %>%
  summarise(Liczebnosc = n(),
            PoziomVSC_przed = round(mean(SredniaPomiar1, na.rm=T), 2),
            PoziomVSC_po = round(mean(SredniaPomiar2, na.rm=T), 2))

Pochodzenie <- as.data.frame(Pochodzenie)[,2:4]
