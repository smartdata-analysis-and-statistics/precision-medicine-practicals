library(survival)
library(lattice)
library(Formula)
library(ggplot2)
library(Hmisc)
library(rms)
library(gridExtra)
library(reshape2)
library(MASS)
library(mgcv)
library(glmnet)
library(pROC)


logit <- function(x){log(x/(1-x))}
expit <- function(x){exp(x)/(1+exp(x))}

df = readRDS('dataset.rds')

model_base = glm(y~x1, data=df, family = 'binomial')
model_base

summary(model_base)

new.patient <- new.patient <- data.frame(x1=-0.3, x2=-0.5, x3=1, x4=1, x5=2)


