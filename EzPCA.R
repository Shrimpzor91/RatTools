#For easy PCA tests. Just import your data frame as "YourData" and the rest of the code is set up.

install.packages("ggplot2")
library(ggplot2)
install.packages("corrr")
library('corrr')
install.packages("FactoMineR")
library("FactoMineR")
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("devtools")
library("devtools")
install_github("kassambara/factoextra")
install.packages("factoextra")
library("factoextra")
library(dplyr)
library(tibble)





YourData <- read.csv() %>% as.numeric

YourStandardizedData <- scale(YourData)

YourCovarianceMatrix = cor(YourStandardizedData)

PlotYourCovarianceMatrix <- ggcorrplot(YourCovarianceMatrix)

YourPrincipleComponentAnalysis <- princomp(PlotYourCovarianceMatrix)
summary(YourPrincipleComponentAnalysis)

#Graph the summary with this
fviz_eig(YourPrincipleComponentAnalysis, addlabels = TRUE)

#change 1:12 to select which components you want to see
Your_PCA_Loadings_Matrix <- YourPrincipleComponentAnalysis$loadings[, 1:12]

# Graphs...

#Biplot of variables on 3D axis
fviz_pca_var(YourPrincipleComponentAnalysis, col.var = "blue")

#Cos Squared line plot of the variables. Axes value = components represented.
fviz_cos2(YourPrincipleComponentAnalysis, choice = "var", axes = 1:12)

#Biplot of Cos Squared variables on 3d Axis.
fviz_pca_var(PCA, col.var = "cos2",
             gradient.cols = c("red", "blue", "yellow"),
             repel = TRUE)


