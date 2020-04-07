# Selecting numerical variables for PCA
nums <- unlist(lapply(data, is.integer)) 
dataPCA <- data[ , nums]
colnames(dataPCA)
dataPCA <- dataPCA[ , !names(dataPCA) %in% c("SkalaRosenberga")]
summary(dataPCA) # 41 individuals, 11 variables


# Calculating PCA
pca <- PCA(dataPCA, graph = FALSE) # data are standarized by default
print(pca)

eig.val <- pca$eig
eig.val

# eigenvalues for PC1-PC3  > 1.0 --> PC3 can be taken as the cut-off point, as 82.6% of the total variance is explained by the first 3 principal components
# An alternative method of estimating the number of principal components is the scree plot, which presents eigenvalues in descending order. The number of PC is selected at the point where there is a relative decrease in the amount of variance explained by the component (Jollife 2002, Peres-Neto, Jackson, and Somers (2005)).


fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50)) 

# RESULTS FOR VARIABLES
var <- get_pca_var(pca)
var

head(var$coord)

# wykres korelacji zmiennych - pokazuje relacje pomiędzy wszystkimi zmiennymi
fviz_pca_var(pca, col.var="steelblue", repel=TRUE)

head(var$cos2) # Cos2 (cosinus kwadratowy, współrzędne kwadratowe): Wartości cos2 służy do oszacowania jakości reprezentacji zmiennych na mapie czynników.

# corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(pca, choice = "var", axes = 1:2)

fviz_pca_var(pca, col.var = "cos2",
             gradient.cols = c("#C2185B", "#C5E1A5", "#1B5E20"), 
             repel = TRUE) 
# Z wykresów jednoznacznie wynika, ze zmienne Waga, Wzrost i Wiek nie wplywaja na przydzial obserwacji do grupy.

head(var$contrib) # Contributions to the principal components

# corrplot(var$contrib, is.corr=FALSE) # aby wyr??ni? najbardziej przyczyniaj?ce si? do wariancji zmienne dla ka?dego wymiaru 

fviz_contrib(pca, choice = "var", axes = 1) # Contributions of variables to PC1
fviz_contrib(pca, choice = "var", axes = 2) # Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 1:2) # Total

# Czerwona przerywana linia oznacza spodziewany u?redniony udzia? zmiennej w wyja?nieniu wariancji. Dla zestandaryzowanych danych = 1/length(variables) = 1/12 = 8,33%. Dla poszczeg?lnych komponent?w, zmienna z udzia?em przewy?szaj?cym t? warto?? mo?e by? rozwa?ana jako znacz?ca.

# Mo?na zauwa?y?, ?e zmienne Waga, Wzrost i Wiek uczestnicz? najsilniej w wyja?nieniu zmienno?ci dla wymiaru 1 i 2.

fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("#C2185B", "#C5E1A5", "#1B5E20"))


res.desc <- dimdesc(pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1
res.desc$Dim.2 # wsz?dzie p-value < 0.05 zatem odrzucamy H0, ?e prawdziwy wsp??czynnik korelacji = 0
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


# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/