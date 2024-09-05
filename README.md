# PCA-analysis

library(tidyverse)
install.packages(c("factoextra", "FactoMineR"))
install.packages("RColorBrewer")
library(RColorBrewer)
devtools::install_github("kassambara/ggpubr")
library(ggpubr)
library("factoextra")
library("FactoMineR")

data <- read.csv("C:/Z-My Drive/Analysis/pcasdu.csv")

pca.data <- PCA(data[,-7], scale.unit = TRUE, graph = FALSE)

summary(pca.data)

fviz_eig(pca.data, addlabels = TRUE, ylim = c(0, 70))

fviz_pca_var(pca.data, col.var = "cos2",
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"),
             repel = TRUE)
pca.data <- PCA(t(data[,-1]), scale.unit = TRUE, graph = FALSE)

fviz_pca_ind(pca.data, col.ind = "cos2",
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"),
             repel = TRUE)
a <- fviz_pca_ind(pca.data, col.ind = "cos2",
                  gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"),
                  repel = TRUE)
ggpar(a,
      title = "Principal Component Analysis",
      xlab = "PC1", ylab = "PC2",
      legend.title = "Cos2", legend.position = "top",
      ggtheme = theme_minimal())

pca.data <- PCA(data[,-1], scale.unit = TRUE,ncp = 2, graph = FALSE)

data$Lineage <- as.factor(data$Lineage)
nb.cols <- 3

mycolors <- colorRampPalette(brewer.pal(3, "Set1"))(nb.cols)

a <- fviz_pca_ind(pca.data, col.ind = data$Lineage,
                  palette = mycolors, addEllipses = TRUE)

ggpar(a,
      title = "Principal Component Analysis",
      xlab = "PC1", ylab = "PC2",
      legend.title = "Cell type", legend.position = "top",
      ggtheme = theme_minimal())

str(pca.data)

pca.data$ind$coord

pca.data$var

pca<-table(pca.data$ind$coord)

df <- data.frame(pca.data$ind$coord)

write.csv(df, "C:/Z-My Drive/postdoc/Analysis/pcasdu2.csv", row.names=FALSE)
