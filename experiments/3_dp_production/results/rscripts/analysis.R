library(ggplot2)
theme_set(theme_bw(18))
setwd("~/webprojects/71_modals_forced_production/results/")
source("rscripts/helpers.r")
load("data/r.RData")

head(r)
nrow(r)

baremust = r[r$response %in% c("bare","must") & r$EvidenceTypeCategorical != "wishful",]
baremust = droplevels(baremust)
nrow(baremust)
table(baremust$response)

centered = cbind(baremust, myCenter(baremust[,c("Directness","EvidenceTypeCategorical")]))
contrasts(centered$response)
contrasts(centered$EvidenceTypeCategorical)
head(centered)

m = glmer(response~cDirectness+EvidenceTypeCategorical + (1|workerid) + (1|item), data=centered, family="binomial")
summary(m)

m.1 = glmer(response~cDirectness*EvidenceTypeCategorical + (1|workerid) + (1|item), data=centered, family="binomial")
summary(m.1)

anova(m, m.1) # interaction not justified
