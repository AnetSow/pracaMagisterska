## PCA ##
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

dane <- read.csv(file="C:/Users/Aneta/Documents/Magisterka/Dane/dane_mgr.csv", header=TRUE, sep=";")

# Wybranie z danych wartoœci numerycznych do PCA
nums <- unlist(lapply(dane, is.integer)) 
danePCA <- dane[ , nums]
summary(danePCA)

danePCA <- danePCA[ , !names(danePCA) %in% c("SkalaRosenberga")]
head(danePCA)

# Za³adowanie bibliotek
library(factoextra)
library(FactoMineR)

# Obliczenie PCA
pca <- PCA(danePCA, graph = FALSE) # domyœlnie dane s¹ standaryzowane automatycznie podczas obliczania PCA
print(pca)

eig.val <- pca$eig
eig.val # wartoœci w³asne (eigenvalues) sk³adowych g³ównych PC1-PC3  > 1.0 --> za punkt odciêcia mo¿na przyj¹æ PC3, gdy¿ 82.6% ca³kowitej warinacji jest wyjaœniana przez pierwsze 3 sk³adowe g³ówne

# Alternatywn¹ metod¹ oszacowania liczby sk³adowych g³ównych jest wykres osypiska, który przedstawia wartoœci w³asne uporz¹dkowane malej¹co. Liczba sk³adowych g³ównych wybierana jest w punkcie, w którym nastêpuje relatywny spadek iloœci wyjaœnianej wariancji przez dan¹ sk³adow¹ (Jollife 2002, Peres-Neto, Jackson, and Somers (2005)).

fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50)) # co z PC.4 ?

# WYNIKI DLA ZMIENNYCH (CECH)
var <- get_pca_var(pca)
var

# koordynaty zmiennych
head(var$coord)
# Korelacja pomiêdzy zmiann¹ a sk³adow¹ g³ówn¹ (PC) s³u¿y jako wspó³rzêdna zmiennej w analizie PCA. Wykres zmiennych ró¿ni siê od wykresu obserwacji tym, ¿e obserwacje s¹ przedstawiane przez ich rzuty, a zmienne s¹ reprezentowane poprzez korelacje (Abdi and Williams 2010). 

# wykres korelacji zmiennych - pokazuje relacje pomiêdzy wszystkimi zmiennymi
fviz_pca_var(pca, col.var="steelblue", repel=TRUE)
# Pozytywnie skorelowane zmienne s¹ zgrupowane razem.
# Negatywnie skorelowane zmienn¹ s¹ po³o¿one na przeciwnych stronach wykresu (w przeciwnych æwiartkach).
# Odleg³oœæ pomiêdzy zmiennymi a pocz¹tkiem uk³ wsp mierzy jakoœæ zmiennych na mapie czynników. Zmienne po³o¿one dalej od pocz¹tku uk³ wsp s¹ dobrze reprezentowane na mapie czynników.

head(var$cos2) # Cos2 (cosinus kwadratowy, wspó³rzêdne kwadratowe): Wartoœci cos2 s³u¿¹ do oszacowania jakoœci reprezentacji zmiennych na mapie czynników.
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(pca, choice = "var", axes = 1:2)
# Wysoka wartoœæ cos2 wskazuje na dobr¹ reprezentacjê zmiennej w sk³adowej g³ównej. W tym przypadku zmienna jest umieszczona blisko obwodu ko³a korelacji (i tym wa¿niejsza jest interpretacja tych sk³adników).

# Niska wartoœæ cos2 wskazuje, ¿e zmienna nie jest dobrze reprezentowana przez PC1. W tym przypadku zmienna znajduje siê blisko œrodka ko³a.

# Dla danej zmiennej suma cos2 wszystkich sk³¹dowych g³ównych jest równa jeden.

# Jeœli zmienna jest doskonale reprezentowana tylko przez dwie sk³adowe g³ówne (Dim.1 i Dim.2), suma cos2 na tych dwóch PC jest równa jeden. W takim przypadku zmienne zostan¹ umieszczone na kole korelacji.

# W przypadku niektórych zmiennych, dla idealnej reprezentacji danych wymagane mog¹ byæ wiêcej ni¿ 2 komponenty. W tym przypadku zmienne s¹ umieszczone wewn¹trz ko³a korelacji.

fviz_pca_var(pca, 
             col.var = "cos2",
             gradient.cols = c("#C2185B", "#C5E1A5", "#1B5E20"), 
             repel = TRUE) 
# Z obu wykresów jednoznacznie wynika, ¿e zmienne Waga, Wzrost i Wiek nie wp³ywaj¹ na przydzia³ obserwacji do grupy.

head(var$contrib) # Contributions to the principal components
# Udzia³ zmiennych w wyjaœnianiu wariancji danej sk³adowej g³ównej wyra¿ony jest w procentach.
# Zmienne, które s¹ skorelowane z PC1 (tj. Dim.1) i PC2 (tj. Dim.2) s¹ najwa¿niejsze w wyjaœnieniu zmiennoœci w zbiorze danych.
# Zmienne, które nie s¹ skorelowane z ¿adnym komponentem lub skorelowane z ostatnimi wymiarami, s¹ zmiennymi o niskim udziale i mog¹ zostaæ usuniête w celu uproszczenia ogólnej analizy.

corrplot(var$contrib, is.corr=FALSE) # aby wyró¿niæ najbardziej przyczyniaj¹ce siê do wariancji zmienne dla ka¿dego wymiaru 

fviz_contrib(pca, choice = "var", axes = 1) # Contributions of variables to PC1
fviz_contrib(pca, choice = "var", axes = 2) # Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 1:2) # Total

# Czerwona przerywana linia oznacza spodziewany uœredniony udzia³ zmiennej w wyjaœnieniu wariancji. Dla zestandaryzowanych danych = 1/length(variables) = 1/12 = 8,33%. Dla poszczególnych komponentów, zmienna z udzia³em przewy¿szaj¹cym tê wartoœæ mo¿e byæ rozwa¿ana jako znacz¹ca.

# Ca³kowity udzia³ danej zmiennej w wyjaœnieniu wariancji przechowywanej przez dwie sk³adowe g³ówne, np PC1 i PC2, oblicza siê jako contrib = [(C1 * Eig1) + (C2 * Eig2)] / (Eig1 + Eig2), gdzie C1 i C2 s¹ udzia³em zmiennej odpowiednio na PC1 i PC2, Eig1 i Eig2 s¹ wartoœciami w³asnymi odpowiednio PC1 i PC2 (Wartoœci w³asne mierz¹ wielkoœæ zmiennoœci przechowywanej przez ka¿d¹ sk³adow¹ g³ówn¹).

# Zatem oczekiwany œredni wk³ad (odciêcie): jeœli wk³ad 10 zmiennych by³by jednolity, oczekiwany œredni wk³ad na PC wyniós³by 1/10 = 10%. Oczekiwany œredni udzia³ zmiennej dla PC1 i PC2 wynosi: [(10 * Eig1) + (10 * Eig2)] / (Eig1 + Eig2)

# Mo¿na zauwa¿yæ, ¿e zmienne Waga, Wzrost i Wiek uczestnicz¹ najsilniej w wyjaœnieniu zmiennoœci dla wymiaru 1 i 2.

fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("#C2185B", "#C5E1A5", "#1B5E20"), repel = TRUE)

res.desc <- dimdesc(pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1
res.desc$Dim.2 # wszêdzie p-value < 0.05 zatem odrzucamy H0, ¿e prawdziwy wspó³czynnik korelacji = 0


# WYNIKI DLA OBSERWACJI
ind <- get_pca_ind(pca)
ind

# Coordinates of individuals
head(ind$coord)
# Quality of individuals
head(ind$cos2)
# Contributions of individuals
head(ind$contrib)

fviz_pca_ind(pca, col.ind = "cos2", pointsize = 2,
             gradient.cols = c("#C2185B", "#C5E1A5", "#1B5E20")) # Obserwacje podobne do siebie tworz¹ zgrupowanie na wykresie.

fviz_cos2(pca, choice = "ind", axes = 1:2)

# danePCA <- mutate(danePCA, Grupa = SredniaPomiar1)
# danePCA$Grupa[danePCA$SredniaPomiar1 <= 100] = "A"
# danePCA$Grupa[danePCA$SredniaPomiar1 > 100 & danePCA$SredniaPomiar1 <= 180] = "B"
# danePCA$Grupa[danePCA$SredniaPomiar1 > 180 & danePCA$SredniaPomiar1 <= 250] = "C"
# danePCA$Grupa[danePCA$SredniaPomiar1 > 250] = "D"

fviz_pca_ind(pca,
             geom.ind = "point", # show points only (but not "text")
             col.ind = danePCA$Grupa, # color by groups
             # palette = c("#C2185B", "#C5E1A5", "#1B5E20"),
             addEllipses = TRUE, ellipse.type = "convex", # concentration/confidence/convex ellipses
             legend.title = "Groups",
             palette = "lancet")


# ggpubr::ggpar(ind.p,
#               title = "Principal Component Analysis",
#               subtitle = "Iris data set",
#               caption = "Source: factoextra",
#               xlab = "PC1", ylab = "PC2",
#               legend.title = "Species", legend.position = "top",
#               ggtheme = theme_gray(), palette = "jco")


# fviz_pca_biplot(pca, 
#                 col.ind = danePCA$Grupa, palette = "lancet", 
#                 addEllipses = TRUE, label = "var",
#                 col.var = "black", repel = TRUE,
#                 legend.title = "Grupy")

