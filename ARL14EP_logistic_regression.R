#load libraries
library(ggpubr)
library(ggplot2)
library(dplyr)

#install.packages("epiDisplay")
library(epiDisplay)
library(oddsratio)

#read in data
pcos <- read.csv("~/Desktop/pcos_article/pcos_levels.csv")

#Change numbering of cases and controls
pcos$Group <- gsub(2, 0, pcos$Group)


#check data clasess
class(pcos$Group)
class(pcos$ARL14EP)

#Change case-control column to numeric
pcos$Group <- as.numeric(pcos$Group)

#remove non-classifc PCOS cases
pcos_classic <- pcos %>% filter(PCOS.phenotype != 0)

#count number of cases and controls
pcos_classic %>% count(Group)

#Is ARL14EP expression associated with PCOS (no correction for testosterone)
logistic_model <- glm(Group ~ ARL14EP + BMI + Age, 
                      data = pcos_classic, 
                      family = "binomial")
summary(logistic_model)

#Calculate P value
(modelChi = logistic_model$null.deviance - logistic_model$deviance)  # for chi sq statistic
(chiDF = logistic_model$df.null - logistic_model$df.residual)        # for DF
(chisq_prob = 1 - pchisq(modelChi, chiDF)) # for hypothesis testin

#Is ARL14EP expression associated with PCOS
logistic_model <- glm(Group ~ ARL14EP + BMI + Age + TESTO, 
                      data = pcos_classic, 
                      family = "binomial")
summary(logistic_model)

#Calculate P value
(modelChi = logistic_model$null.deviance - logistic_model$deviance)  # for chi sq statistic
(chiDF = logistic_model$df.null - logistic_model$df.residual)        # for DF
(chisq_prob = 1 - pchisq(modelChi, chiDF)) # for hypothesis testin

#Visulaize results
ggplot(pcos_classic,aes(x=ARL14EP,y=TESTO, color=factor(Group)))+geom_point()+geom_smooth(method = "lm")




