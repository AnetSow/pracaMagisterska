library(tsne)
library(Rtsne)


data <- read.csv(file="./Data/dane_mgr.csv", header=TRUE, sep=";")

# Selecting numerical variables for t-SNE
nums <- unlist(lapply(data, is.integer)) 
data_tSNE <- data[ , nums]
data_tSNE <- data_tSNE[ , !names(data_tSNE) %in% c("SkalaRosenberga")]
new_names <- c("Age", "Meas.Ia", "Meas.Ib", "Meas.Ic", "AvgMeas.I", "Meas.IIa", "Meas.IIb", "Meas.IIc", "AvgMeas.II", "Height", "Weight")
colnames(data_tSNE) <- new_names
summary(data_tSNE)


## Curating the database for analysis with both t-SNE and PCA
Labels<-colnames(data_tSNE)
colnames(data_tSNE)<-as.factor(colnames(data_tSNE))
## for plotting
colors = rainbow(length(unique(colnames(data_tSNE))))
names(colors) = unique(colnames(data_tSNE))

## Executing the algorithm on curated data
tsne <- Rtsne(data_tSNE[,2:9], dims = 2, perplexity=10, verbose=TRUE, max_iter = 500)
# exeTimeTsne<- system.time(Rtsne(data_tSNE[,2:9], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500))

## Plotting
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=colnames(data_tSNE), col=colors[colnames(data_tSNE)])


##################################


# k-means
km <- kmeans(data_tSNE, centers = 5)

# attaching classes to data
data_tSNE$cluster <- as.factor(km$cluster)

# parameters
perplex <- 5; inter <- 5000; dims <- ncol(data_tSNE) 
# dims = 2, initial_dims = 50,
#   perplexity = 30, theta = 0.5, check_duplicates = TRUE,
#   pca = TRUE, partial_pca = FALSE, max_iter = 1000,
#   verbose = getOption("verbose", FALSE), is_distance = FALSE,
#   Y_init = NULL, pca_center = TRUE, pca_scale = FALSE,
#   normalize = TRUE, stop_lying_iter = ifelse(is.null(Y_init), 250L,
#   0L), mom_switch_iter = ifelse(is.null(Y_init), 250L, 0L),
#   momentum = 0.5, final_momentum = 0.8, eta = 200,
#   exaggeration_factor = 12, num_threads = 1, ...


# t-SNE calculations
tsne <- Rtsne(data_tSNE, perplexity=perplex, initial_dims = ncol(data_tSNE))

# points in a 2D plane

tsne_df <- as.data.frame(tsne$Y)

ggplot(tsne_df, aes(V1, V2)) + 
    geom_point()

# clusters in a 2D plane

tsne_df$cluster <- as.factor(data_tSNE$cluster)

ggplot(tsne_df, aes(V1, V2, color = cluster)) + 
    geom_point()
