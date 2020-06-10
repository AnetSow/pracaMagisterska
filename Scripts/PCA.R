library(factoextra)
library(FactoMineR)

# Selecting numerical variables for PCA
nums <- unlist(lapply(data, is.integer)) 
data_PCA <- data[ , nums]
colnames(data_PCA)
data_PCA <- data_PCA[ , !names(data_PCA) %in% c("SkalaRosenberga")]
new_names <- c("Age", "Meas.Ia", "Meas.Ib", "Meas.Ic", "AvgMeas.I", "Meas.IIa", "Meas.IIb", "Meas.IIc", "AvgMeas.II", "Height", "Weight")
colnames(data_PCA) <- new_names
summary(data_PCA) # 41 individuals, 11 variables


# Calculating PCA
pca <- PCA(data_PCA, graph = FALSE) # data are standarized by default
# print(pca)

eig_values <- pca$eig
eig_values

# eigenvalues for PC1-PC3  > 1.0 --> PC3 can be taken as the cut-off point, as 82.6% of the total variance is explained by the first 3 principal components
# An alternative method of estimating the number of principal components is the scree plot, which presents eigenvalues in descending order. The number of PC is selected at the point where there is a relative decrease in the amount of variance explained by the component (Jollife 2002, Peres-Neto, Jackson, and Somers (2005)).


fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50)) 
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50), ncp = 11, barfill = "skyblue2",
         barcolor = "black", ggtheme = theme_classic()) 
# figure_saving("3.2")


# RESULTS FOR VARIABLES
var <- get_pca_var(pca)
var

var$coord

# wykres korelacji zmiennych - pokazuje relacje pomiędzy wszystkimi zmiennymi
fviz_pca_var(pca, col.var="steelblue", repel=TRUE)

head(var$cos2) # Cos2 (cosinus kwadratowy, współrzędne kwadratowe): Wartości cos2 służy do oszacowania jakości reprezentacji zmiennych na mapie czynników.

# corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(pca, choice = "var", axes = 1:2)

fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("#C2185B", "#C5E1A5", "#1B5E20"), 
             repel = TRUE) 
# Z wykresów jednoznacznie wynika, ze zmienne Waga, Wzrost i Wiek nie wplywaja na przydzial obserwacji do grupy.

var$contrib # Contributions to the principal components

# corrplot(var$contrib, is.corr=FALSE) # aby wyr??ni? najbardziej przyczyniaj?ce si? do wariancji zmienne dla ka?dego wymiaru 

fviz_contrib(pca, choice = "var", axes = 1) # Contributions of variables to PC1
fviz_contrib(pca, choice = "var", axes = 2) # Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 1:2) # Total

# Czerwona przerywana linia oznacza spodziewany u?redniony udzia? zmiennej w wyja?nieniu wariancji. Dla zestandaryzowanych danych = 1/length(variables) = 1/12 = 8,33%. Dla poszczeg?lnych komponent?w, zmienna z udzia?em przewy?szaj?cym t? warto?? mo?e by? rozwa?ana jako znacz?ca.

# Mo?na zauwa?y?, ?e zmienne Waga, Wzrost i Wiek uczestnicz? najsilniej w wyja?nieniu zmienno?ci dla wymiaru 1 i 2.

fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("#C2185B", "#C5E1A5", "#1B5E20"))


res_desc <- dimdesc(pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res_desc$Dim.1
res_desc$Dim.2 # wsz?dzie p-value < 0.05 zatem odrzucamy H0, ?e prawdziwy wsp??czynnik korelacji = 0
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
             gradient.cols = c("#C2185B", "#C5E1A5", "#1B5E20")) # Obserwacje podobne do siebie tworz? zgrupowanie na wykresie.

fviz_cos2(pca, choice = "ind", axes = 1:2)

# data_PCA <- mutate(data_PCA, Grupa = SredniaPomiar1)
# data_PCA$Grupa[data_PCA$SredniaPomiar1 <= 100] = "A"
# data_PCA$Grupa[data_PCA$SredniaPomiar1 > 100 & data_PCA$SredniaPomiar1 <= 180] = "B"
# data_PCA$Grupa[data_PCA$SredniaPomiar1 > 180 & data_PCA$SredniaPomiar1 <= 250] = "C"
# data_PCA$Grupa[data_PCA$SredniaPomiar1 > 250] = "D"

fviz_pca_ind(pca,
             geom.ind = "point", # show points only (but not "text")
             col.ind = VSC_before$Grupa, # color by groups
             palette = c("#C2185B", "#C5E1A5", "#1B5E20"),
             addEllipses = TRUE, 
             # ellipse.type = "confidence", # concentration/confidence/convex ellipses
             legend.title = "Groups")


fviz_pca_biplot(pca, 
                # Individuals
                geom.ind = "point", 
                col.ind = VSC_before$Grupa,
                fill.ind = "black",
                label = "all",  
                palette = c("#d73027", "#91cf60", "#1a9850"),
                addEllipses = TRUE,
                
                # Variables
                geom.var = c("arrow", "text"), 
                col.var = "steelblue", 
                gradient.cols = NULL,
                
                repel = TRUE,
                ggtheme = theme_classic(),
                
                legend.title = "Groups",
                title = " ")

figure_saving("3.3.2")

# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/



group <- c(rep("B", times=24),  rep("C", times=3),rep("A", times=14))

fviz_pca_biplot(pca, 
                # Variables
                col.var="steelblue", 
                arrowsize=0.6, 
                labelsize=5, 
                # Individuals
                geom.ind = "point",  
                col.ind=VSC_before$Grupa, 
                palette=c("#d73027", "#91cf60", "#1a9850"), 
                addEllipses=TRUE, ellipse.type="confidence", 
                
                repel=TRUE, ggtheme = theme_classic(), legend.title = "Groups", title = " ")

figure_saving("3.3.3")


# Classification checking - before tonsillectomy


VSC_before_pca <- select(data, Wiek, PomiarIa, PomiarIb,PomiarIc, SredniaPomiar1, Wzrost, Waga)
colnames(VSC_before_pca) <- c("Age", "Meas.Ia", "Meas.Ib", "Meas.Ic", "AvgMeas.I", "Height", "Weight")

VSC_before_pca <- mutate(VSC_before_pca, Group = AvgMeas.I)

VSC_before_pca$Group[VSC_before_pca$AvgMeas.I <= 100] = 1
VSC_before_pca$Group[VSC_before_pca$AvgMeas.I > 100 & VSC_before_pca$AvgMeas.I <= 180] = 2
VSC_before_pca$Group[VSC_before_pca$AvgMeas.I > 180 & VSC_before_pca$AvgMeas.I <= 250] = 3
VSC_before_pca$Group[VSC_before_pca$AvgMeas.I > 250] = 4


pca_before <- PCA(X = VSC_before_pca, graph = T)



pca$call
pca$call$row.w #      "weights for the individuals"
pca$call$col.w # "weights for the variables"

df.projected <- as.data.frame(predict(pca$X, ))