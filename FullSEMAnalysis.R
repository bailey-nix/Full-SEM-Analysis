## Full SEM Path Analysis

#load libraries 
library(tidyverse)
library(lavaan)
library(semTools)
library(semPlot)


#entering std deviations and correlations
sd<-c(.939, 1.017, .937, .562, .760, .524, .585, .609, .731, .711, 1.124 ,1.001)
corr <- '
1.000
  .668 1.000
  .635  .599 1.000
  .263  .261  .164 1.000
  .290  .315  .247  .486 1.000
  .207  .245  .231  .251  .449 1.000
 -.206 -.182 -.195 -.309 -.266 -.142 1.000 
 -.280 -.241 -.238 -.344 -.305 -.230  .753 1.000
 -.258 -.244 -.185 -.255 -.255 -.215  .554  .587 1.000 
  .080  .096  .094 -.017  .151  .141 -.074 -.111  .016 1.000
  .061  .028 -.035 -.058 -.051 -.003 -.040 -.040 -.018 .284 1.000
  .113  .174  .059  .063  .138  .044 -.119 -.073 -.084 .563  .379 1.000
  '
corr<-getCov(corr, names = c('Y7', 'Y8', 'Y9', 'Y4', 'Y5', 'Y6', 'Y1', 'Y2', 'Y3', 'X1', 'X2', 'X3'))
#creating covariance matrix
dat<- cor2cov(corr,sd)


#testing CFA model
#defining model
model_cfa<-'
job=~Y7+Y8+Y9

subj_wb=~Y4+Y5+Y6

dysf=~Y1+Y2+Y3

cons_th=~X1+X2+X3

#factors are correlated
job~~subj_wb
job~~dysf
job~~cons_th

subj_wb~~dysf
subj_wb~~cons_th

dysf~~cons_th
'

#fitting model
cfa<-cfa(model_cfa,sample.cov=dat, sample.nobs=263)
#fit indices and parameters
summary(cfa,fit.measures=TRUE, standardized=TRUE)

cov2cor(lavInspect(cfa, what = "est")$psi)

#modification indices
modindices(cfa, sort.=TRUE)

#calculate omega reliability coefficient 
compRelSEM(cfa)


#test FULL model
full_model<-'
#cfa
job=~Y7+Y8+Y9
subj_wb=~Y4+Y5+Y6
dysf=~Y1+Y2+Y3
cons_th=~X1+X2+X3

#path model
subj_wb~beta21*dysf
job~beta32*subj_wb+beta31*dysf

#covariance between exogenous variables
dysf~gamma11*cons_th

#mediation effects
ind_job_subjwb_dysf_consth:=beta32*beta21*gamma11
ind_job_dysf_consth:=beta31*gamma11
ind_job_consth:=beta21*gamma11
'

#fitting model
full<-sem(full_model,sample.cov=dat,sample.nobs=263)
#indices and parameters
summary(full,fit.measures=TRUE, standardized=TRUE)


#calculating the 95% CI of the mediation effects
monteCarloCI(full)


#plotting the models
semPaths(full)
semPaths(cfa)
