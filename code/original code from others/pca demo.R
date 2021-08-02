data(decathlon2)
decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6])
res.pca <- prcomp(decathlon2.active, scale = TRUE)
fviz_eig(res.pca)

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 


data(iris)
rest.pca <- prcomp(iris[, -5],  scale = TRUE)

# Extract eigenvalues/variances
get_eig(rest.pca)

con.scaled2<-apply(concern_scores, 2, scale)
con.covar<-cov(con.scaled2)
con.eig<-eigen(con.covar)
str(con.eig)

con.covar2<-cov(concern_scores_rescaled[,1:6])
con.eig2<-eigen(con.covar2)
str(con.eig2)