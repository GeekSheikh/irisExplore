# Neat Tutorial on regression
# ---------------------------
# Use this [link](http://scg.sdsu.edu/linear-regression-in-r-abalone-dataset/)
require(devtools)
install_version("ggplot2", version = "2.2.0")
install.packages("AppliedPredictiveModeling")
install.packages("MASS")
install.packages("rms")
library("AppliedPredictiveModeling")
library(dplyr)
library(MASS)
library(rms)
library(ggplot2)
?AppliedPredictiveModeling

data(abalone)

?abalone
abalone
?lm
#Notice issue with min height is 0 - that's a problem
summary(abalone)
abalone[abalone$Height==0.0,]
#Set these to null
abalone[abalone$Height==0.0,] == NA

#Low weights as well
summary(abalone)
abalone[abalone$WholeWeight < .01,]
hist(abalone$WholeWeight)

#Create the normalized matrix
as.matrix(cor(na.omit(abalone[,-1])))

# Build the first Regression Models
# ---------
abfit1 = lm(Rings ~ Type + LongestShell + Diameter + Height + WholeWeight
               + ShuckedWeight + VisceraWeight + ShellWeight, data = abalone)
abfit2 = stepAIC(abfit1)
summary(abfit2)

abalone$Type = as.character(abalone$Type)
abalone$Type[abalone$Type != 'I'] = 'K'
abalone$Type = as.factor(abalone$Type)

abalone$weight.diff = abalone$WholeWeight - 
      (abalone$VisceraWeight + abalone$ShuckedWeight + abalone$ShellWeight)
par(mfrow=c(1,1))
hist(abalone$weight.diff,breaks=50)

# Get rid of the instances where the whole weight is < the sum of its weights
abalone[abalone$weight.diff < -.1,]

abfit2.1 = lm(Rings ~ Type + Diameter + Height + WholeWeight, data = abalone)
abfit2.2 = lm(Rings ~ Type + Diameter + Height + ShellWeight, data = abalone)
abfit2.3 = lm(Rings ~ Type + Diameter + Height + VisceraWeight, data = abalone)
abfit2.4 = lm(Rings ~ Type + Diameter + Height + ShellWeight, data = abalone)

par(mfrow=c(2,2))
plot(abfit2.2)

abalone[2052,]
summary(abalone[abalone$sex=='K',])

abalone$Height[2052] = 0.130
abalone$Weight.mean1 = (abalone$ShuckedWeight*abalone$VisceraWeight*abalone$ShellWeight)^(1/3)
abalone$Weight.mean2 = (abalone$WholeWeight*abalone$ShuckedWeight*abalone$ShellWeight*abalone$VisceraWeight)^(1/4)
abalone$Weight.norm1 = sqrt(abalone$ShuckedWeight^2 + abalone$VisceraWeight^2 + abalone$ShellWeight^2)
abalone$Weight.norm2 = sqrt(abalone$WholeWeight^2 + abalone$ShuckedWeight^2 + abalone$VisceraWeight^2 + abalone$ShellWeight^2)

abalone$size.norm = sqrt(abalone$LongestShell^2 + abalone$Diameter^2 + abalone$Height^2) # Norm of vectors
abalone$size.mean = (abalone$LongestShell*abalone$Diameter*abalone$Height)^(1/3)         # Geometric Mean


abfit3 = lm(Rings ~ Type + size.norm + WholeWeight + ShuckedWeight + VisceraWeight + ShuckedWeight, data = abalone)
 
abfit3.1 = lm(Rings ~ Type + size.norm + WholeWeight, data = abalone)
abfit3.2 = lm(Rings ~ Type + size.norm + ShuckedWeight, data = abalone)
abfit3.3 = lm(Rings ~ Type + size.norm + VisceraWeight, data = abalone)
abfit3.4 = lm(Rings ~ Type + size.norm + ShuckedWeight, data = abalone) # best
 
abfit4.1 = lm(Rings ~ Type + size.norm + Weight.norm1, data = abalone)
abfit4.2 = lm(Rings ~ Type + size.norm + Weight.norm2, data = abalone)
abfit4.3 = lm(Rings ~ Type + size.norm + Weight.mean1, data = abalone)
abfit4.4 = lm(Rings ~ Type + size.norm + Weight.mean2, data = abalone)
 
abfit5.1 = lm(Rings ~ Type + size.mean + Weight.norm1, data = abalone)
abfit5.2 = lm(Rings ~ Type + size.mean + Weight.norm2, data = abalone)
abfit5.3 = lm(Rings ~ Type + size.mean + Weight.mean1, data = abalone)
abfit5.4 = lm(Rings ~ Type + size.mean + Weight.mean2, data = abalone)
 
abfit6 = lm(Rings ~ Type + size.mean + WholeWeight + ShuckedWeight + VisceraWeight + ShuckedWeight, data = abalone)
abfit6.1 = lm(Rings ~ Type + size.mean + ShuckedWeight + VisceraWeight + ShuckedWeight, data = abalone)
abfit6.2 = lm(Rings ~ Type + size.mean + ShuckedWeight + ShuckedWeight, data = abalone)
abfit6.3 = lm(Rings ~ Type + size.mean + I(size.mean^2) + ShuckedWeight + ShuckedWeight, data = abalone)

par(mfrow=c(2,2))
plot(abfit6.3)

abfit7 = lm(log(Rings) ~ Type + size.mean + ShuckedWeight + ShellWeight , data = abalone)
plot(abfit7)

abalone[abalone$Rings == NA,]
log(abalone$Rings)
abfit7.1 = lm(log(Rings) ~ Type + size.mean + I(size.mean^2) + ShuckedWeight + ShellWeight , data = abalone)
abfit7.2 = lm(log(Rings) ~ Type + log(size.mean) + ShuckedWeight + ShellWeight , data = abalone)
abfit7.3 = lm(log(Rings) ~ Type + log(size.mean) + ShuckedWeight * ShellWeight , data = abalone)
abfit7.4 = lm(log(Rings) ~ Type + log(size.mean) + WholeWeight, data = abalone)

par(mfrow=c(2,2))
plot(abfit7.1)

?filter