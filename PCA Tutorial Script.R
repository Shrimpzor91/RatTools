
#Principle Component Analysis is extremely simple and easy with R. In this tutorial I want to explain,
#Step by step, how to get from a data set to a complete PCA and graph the results.
#I will show how to standardize the data, make the covariance matrix, and run the PCA itself.
#It's surprisingly uncomplicated with the right packages, but without them it's another story.
#Finally I will try to explain what the metrics mean and how to make visual representations of the results.
#This information was compiled from various sources including github, datacamp, and R Bloggers, as well as the PCA wikipedia article.
#Enjoy my summary of what was honestly a pretty intimidating journey. But I made it and so will you.



#These are the packages and libraries to install to preform a PCA test and related functions
#ggplot2 is needed to load ggcorplot, which we use to visualize covariance matrices
#corrr is used to make those matrices
#The PCA itself is made with FactoMiner, which is the only truly necessary package for this task.



install.packages("ggplot2")
library(ggplot2)
install.packages("corrr")
library('corrr')
install.packages("FactoMineR")
library("FactoMineR")
install.packages("ggcorrplot")
library(ggcorrplot)


#this is for visualizing the PCA in graphs
#the devtools library is required to install the factoextra package which allows you to make the graphs.


install.packages("devtools")
library("devtools")
install_github("kassambara/factoextra")
install.packages("factoextra")
library("factoextra")

#These are packages that help with data cleaning in general.


library(dplyr)
library(tibble)

#This is just a sample data set I created using the sample function to demonstrate the PCA test
#each variable here represents a potential variable in our data set, but as a vector of intigers.
#there are 100 data points for each with varying quantities at different magnitudes 
#so the first task is to sort variables into a standardised matrix.


x <- sample(1:2,100,replace = TRUE)
y <- sample(34:65,100,replace = TRUE)
z <- sample(500:600,100,replace = TRUE)
x1 <- sample(20:34,100,replace = TRUE)
y1 <- sample(99:104,100,replace = TRUE)
z1 <- sample(100000:900000,100,replace = TRUE)
x2 <- sample(1:9,100,replace = TRUE)
y2 <- sample(20:34,100,replace = TRUE)
z2 <- sample(1:9,100,replace = TRUE)
x3 <- sample(500:600,100,replace = TRUE)
y3 <- sample(20:34,100,replace = TRUE)
z3 <- sample(1:9,100,replace = TRUE)

#We now create a 12 variable data frame that's 100 rows long.


xdata <- data.frame(x,y,z,x1,y1,z1,x2,y2,z2,x3,y3,z3)


#To standardize data for our covariance matrix manually we need the standard deviation and mean of each variable

colsd = sapply(xdata, sd)
colmeans = colMeans(xdata)


#We then subtract the mean of from each variable and divide the difference using the standard deviation of that variable
#this produces a value for each variable 


standardizedxdata <- (xdata- colmeans)/colsd

#This previous two steps can actually be bypassed using a function called "scale" which does exactly what we just did to the data set.

SimpleSDXData <- scale(xdata)

#There are multiple ways to make a covariance matrix. Corrr has the "cor" function which can do this, but base R has the "cov" function
#either can be used for this and I'm not entirely sure what the difference is.

CovarianceMatrix = cor(standardizedxdata )
CovarianceMatrix2 = cov(standardizedxdata)

#The following function can plot out your matrix in a visual way so that you see where the high correlations are.
#it has additional arguments for extra features but we don't really care about this for PCA

ggcorrplot(CovarianceMatrix ) 

#Now we can jump straight to the actual PCA itself. Princomp is the "Principal component analysis" function in the FactoMiner package.
#All the complicated stuff with Eigen vectors is done by the computer here. We just make the cov-matrix and it does the rest.
#Summary can let us see the result
#Each component is listed from the most impactful to the least, and there are three properties listed.
#Proportion of variance is how much of the total variance in the data set can be attributed to that component.
#Cumulative variance is what total amount of variance is accounted for by components up to that point (remember they are in descending order of impact)
#You will see the cumulative variance increase with each component until it hits 1 on the last component.
#Standard deviation shows the impact of that component as a distance from an imagined normal curve.
#The most important number is this Proportion of Variance, which tells us how much of the correlation a component is responsible for.


PCA <- princomp(CovarianceMatrix)
summary(data.pca)

#The summary table can be graphed with this function.

fviz_eig(PCA, addlabels = TRUE)

#So that's lovely and all but what is component 1? Well to see what the components actually mean we have to use
#the loadings function. This creates a 2 dimensional array with all the coefficients for each variable that
#define the Eigen vectors.The code below creates a matrix of all variables and how they relate to the components.
#Large positive numbers are the main predictors of a component and result in most of the variance in our data set.
#so if we look at component 1 on this matrix and order the numbers from highest to lowest value, we can see
#that variables z3 and z2 are responsible for the largest proportion of component 1.


PCA_Loadings_Matrix <- PCA$loadings[, 1:12]

#Now lets make some more fancy graphs and stuff with this new-found toy.
#Here are a few different ways to visualize aspects of PCA

#The first graph is a biplot of the variables 
#This is an attempt to show clustered variables on a 3 dimensional axis space that approximates the Eigenvectors and Eigenvalues.
#Variables that cluster together have similar directions (note the positive and negative axis labels)
#The length of a line represents the values while the direction represents the vector.


fviz_pca_var(PCA, col.var = "blue")

#Mathematically we gauge the proportional representation of a variable in a component with the cosign squared (cos2)
#To graph these numbers we use the following function. The choice argument defines what aspect of the PCA test to graph the cos2 of.
#The axes argument, which is defined as dimensions in the graph title, represents which components to graph. 
#the first graph is only the first component. The second graph is only the second, and the third graph is all components.

fviz_cos2(PCA, choice = "var", axes = 1)
fviz_cos2(PCA, choice = "var", axes = 2)
fviz_cos2(PCA, choice = "var", axes = 1:12)


#Finally we have a combination of the biplot and cos2 graphs by modifying fviz_pca_var with var = "cos2"
fviz_pca_var(PCA, col.var = "cos2",
             gradient.cols = c("red", "blue", "yellow"),
             repel = TRUE)


#And that's it. That's all I was able to find out on this topic without going too deep into the math.
#You can now run this statistical model on any data set and represent the results visually.Have a nice day.