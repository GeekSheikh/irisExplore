devtools::install_github("rstudio/sparklyr")
install.packages(c("nycflights13", "Lahman"))
install.packages("mgcv")
install.packages("ggvis")
install.packages("apply_props")
install.packages("class")
install.packages("gmodels")
install.packages("RCurl")
library(gmodels)
library(class)
library(sparklyr)
library(dplyr)
library(ggplot2)
library(ggvis)
sc <- spark_connect(master = "yarn-client")
iris
iris_tbl <- copy_to(sc, iris, "iris", overwrite = TRUE)
iris_tbl
iris_tbl %>% ~Species
iris_tbl %>%
	ggvis(select(Sepal_Length), select(Sepal_Width)) %>% 
	layer_points()

iris %>%
	ggvis(~Sepal.Length, ~Petal.Length, fill = ~Species) %>% 
	layer_points()

iris %>% 
	ggvis(~Sepal.Length, ~Petal.Length, fill = ~Species) %>% 
	layer_points()


##Summarizations
?summarise
head(iris)
summary(iris[c("Petal.Width", "Sepal.Width")])
summarise(iris_tbl, mean_pdl_w = mean(Petal_Width), mean_spl_w = mean(Sepal_Width))

#Taking Samples
?sample
iris_sample <- sample_n(iris_tbl, 70) %>% collect()
iris_sample <- sample_frac(iris_tbl, .01) %>% collect()
iris_sample

iris_sample %>%
	ggvis(~Sepal_Length, ~Petal_Length, fill = ~Species) %>% 
	layer_points()

iris_sample %>%
	ggvis(~Sepal_Length, ~Petal_Length, fill = ~Species) %>% 
	layer_points()


##Creating Test & Training Data Sets
set.seed(1873)
ind <- sample(2, nrow(iris_tbl), replace = TRUE, prob=c(0.67, 0.33))
iris_training <- iris[ind==1, 1:4]
iris_test <- iris[ind==2, 1:4]

iris
iris_training
iris_test

##Create Test & Training Labels
iris_trainLabels <- iris[ind==1, 5]
iris_testLabels <- iris[ind==2, 5]


iris_pred <- knn(train = iris_training, test = iris_test, cl = iris_trainLabels, k=3)
iris_pred

CrossTable(x = iris_testLabels, y = iris_pred, prop.chisq=FALSE)


# Clustergram
# -----------
source("https://raw.github.com/talgalili/R-code-snippets/master/clustergram.r")
par(cex.lab = 1.5, cex.main = 1.2)
iris_scale <- scale(iris[,-5])
iris_scale
clustergram(iris_scale, k.range = 2:8, line.width = 0.004)

set.seed(250)
Data <- rbind(
		cbind(rnorm(100,1, sd = 0.3),rnorm(100,0, sd = 0.3),rnorm(100,0, sd = 0.3),rnorm(100,0, sd = 0.3)),
		cbind(rnorm(100,0, sd = 0.3),rnorm(100,1, sd = 0.3),rnorm(100,0, sd = 0.3),rnorm(100,0, sd = 0.3)),
		cbind(rnorm(100,0, sd = 0.3),rnorm(100,1, sd = 0.3),rnorm(100,1, sd = 0.3),rnorm(100,0, sd = 0.3)),
		cbind(rnorm(100,0, sd = 0.3),rnorm(100,0, sd = 0.3),rnorm(100,0, sd = 0.3),rnorm(100,1, sd = 0.3))
		)
clustergram(Data, k.range = 2:8 , line.width = .004, add.center.points = T)







