install.packages("AppliedPredictiveModeling")
library("AppliedPredictiveModeling")
library(dplyr)
?AppliedPredictiveModeling

data(abalone)
?abalone
abalone
?lm

?lm
lm(formula = Type ~ Diameter, data = abalone)

m_ab <- filter(abalone, Type == "M")
f_ab <- filter(abalone, Type == "F")

lm(formula = LongestShell ~ Diameter, data = m_ab)


?filter