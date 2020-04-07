## MICROBIOLOGICAL RESEARCH ##

# A function returns all bacterial strain names in a given column
get_bacterial_strains = function(x) {
  strains <- x[x != "ujemny"]
  strains <- paste(strains, collapse = ", ")
  strains <- unlist(strsplit(strains, ","))
  strains <- trimws(strains)
  return(strains)
}

# for both groups
anaerobes_both <- get_bacterial_strains(data$WymazMigdalkiBeztlenowe)
aerobic_both <- get_bacterial_strains(data$WymazMigdalkiTlenowe)

length(unique(anaerobes_both))
length(unique(aerobic_both))


# dla grupy kontrolnej PRZED OPERACJ?

wymazMigd <- data[, c('SredniaPomiar1', 'SredniaPomiar2', 'WymazMigdalkiBeztlenowe','WymazMigdalkiTlenowe')]
wymazMigd_Kontrola <- wymazMigd[ wymazMigd$SredniaPomiar1 <= 100, ]

beztlenoweMigd_Kontrola <- get_bacterial_strains(wymazMigd_Kontrola$WymazMigdalkiBeztlenowe)
tlenoweMigd_Kontrola <- get_bacterial_strains(wymazMigd_Kontrola$WymazMigdalkiTlenowe)

length(beztlenoweMigd_Kontrola) # 8
length(tlenoweMigd_Kontrola) # 39


# dla grupy badawczej PRZED OPERACJ?

wymazMigd_Halitoza <- wymazMigd[ wymazMigd$SredniaPomiar1 > 100, ]

beztlenoweMigd_Halitoza <- get_bacterial_strains(wymazMigd_Halitoza$WymazMigdalkiBeztlenowe)
tlenoweMigd_Halitoza <- get_bacterial_strains(wymazMigd_Halitoza$WymazMigdalkiTlenowe)

length(beztlenoweMigd_Halitoza) # 2
length(tlenoweMigd_Halitoza) # 27


# dla grupy kontrolnej PO OPERACJI

wymazMigd_Kontrola2 <- wymazMigd[ wymazMigd$SredniaPomiar2 <= 100, ]

beztlenoweMigd_Kontrola2 <- get_bacterial_strains(wymazMigd_Kontrola2$WymazMigdalkiBeztlenowe)
tlenoweMigd_Kontrola2 <- get_bacterial_strains(wymazMigd_Kontrola2$WymazMigdalkiTlenowe)

length(beztlenoweMigd_Kontrola2) # 0
length(tlenoweMigd_Kontrola2) # 3

# dla grupy badawczej PO OPERACJI
wymazMigd_Halitoza2 <- wymazMigd[ wymazMigd$SredniaPomiar2 > 100, ]

beztlenoweMigd_Halitoza2 <- get_bacterial_strains(wymazMigd_Halitoza2$WymazMigdalkiBeztlenowe)
tlenoweMigd_Halitoza2 <- get_bacterial_strains(wymazMigd_Halitoza2$WymazMigdalkiTlenowe)

length(beztlenoweMigd_Halitoza2) # 10
length(tlenoweMigd_Halitoza2) # 63


# Tabela przedstawiaj?ca liczb? r??nych szczep?w bakterii/grzyb?w

data.frame("Grupa"=c("beztlenowe","tlenowe"),
           "Norma_PRZED"=c(length(beztlenoweMigd_Kontrola), length(tlenoweMigd_Kontrola)),
           "Halitoza_PRZED"=c(length(beztlenoweMigd_Halitoza), length(tlenoweMigd_Halitoza)),
           "Norma_PO"=c(length(beztlenoweMigd_Kontrola2), length(tlenoweMigd_Kontrola2)),
           "Halitoza_PO"=c(length(beztlenoweMigd_Halitoza2), length(tlenoweMigd_Halitoza2)),
           "Razem"=c(length(anaerobes_both), length(aerobic_both)))

