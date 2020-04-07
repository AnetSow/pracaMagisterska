dane <- read.csv(file="wymazy.csv", header=TRUE, sep=",")

wymazy <- dane[ dane$SredniaPomiar1 > 100, ]


# spr.szczepy = function(x) {
#   szczepy <- c()
#   for (val in x){
#     if (val != "ujemny") {
#       szczepy <- c(szczepy, val) } }
#   szczepy <- paste(szczepy, collapse = ", ")
#   szczepy <- unlist(strsplit(szczepy, ","))
#   szczepy <- trimws(szczepy)
#   return(szczepy)
# }

spr.szczepy = function(x) {
  szczepy <- x[x != "ujemny"]
  szczepy <- paste(szczepy, collapse = ", ")
  szczepy <- unlist(strsplit(szczepy, ","))
  szczepy <- trimws(szczepy)
  return(szczepy)
}

beztlenowe <- spr.szczepy(wymazy[,2])
tlenowe <- spr.szczepy(wymazy[,3])
grzyby <- spr.szczepy(wymazy[,4])

length(unique(grzyby))
# [1] "Candida albicans"  " Candida albicans"
