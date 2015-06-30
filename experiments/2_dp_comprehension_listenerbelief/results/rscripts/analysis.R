library(ggplot2)
theme_set(theme_bw(18))
#setwd("~/webprojects/70_modals_comprehension_evidence/results/")
setwd("~/cogsci/projects/stanford/projects/modals/modals/experiments/72_modals_comprehension_evidence_room/results/")
source("rscripts/helpers.r")
load("data/r.RData")

## do analysis jointly on this dataset and the one from experiment 70, that differed only in  "windowless room" being replaced by "windowless office"
exp2 = r
exp2$workerid = exp2$workerid + 60
load("~/cogsci/projects/stanford/projects/modals/modals/experiments/70_modals_comprehension_evidence/results/data/r.RData")
r = rbind(exp2, r)
head(r)

baremust = r[r$item_type %in% c("bare","must"),]
baremust = droplevels(baremust)
nrow(baremust)
table(baremust$item_type)

centered = cbind(baremust, myCenter(baremust[,c("item_type","Directness","EvidenceDirectnessCategorical")]))
contrasts(centered$item_type)
head(centered)

## analysis of p(rain) after bare vs "must"
m = lmer(response~citem_type + (1|workerid) + (1|item), data=centered)
summary(m)

# means
agr = aggregate(response~item_type,FUN=mean,data=r)
agr$SD = aggregate(response~item_type,FUN=sd,data=r)$response
agr

## analysis of evidence after bare vs "must"
m = lmer(Directness~citem_type + (1|workerid) + (1|item), data=centered)
summary(m)

agr = aggregate(Directness~item_type,FUN=mean,data=r)
agr$SD = aggregate(Directness~item_type,FUN=sd,data=r)$Directness
agr


## same thing but with p values
library(lmerTest)
## analysis of p(rain) after bare vs "must"
m = lmer(response~citem_type + (1|workerid) + (1|item), data=centered)
summary(m)

## analysis of evidence after bare vs "must"
m = lmer(Directness~citem_type + (1|workerid) + (1|item), data=centered)
summary(m)
