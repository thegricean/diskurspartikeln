library(ggplot2)
theme_set(theme_bw(18))
#setwd("~/webprojects/70_modals_comprehension_evidence/results/")
setwd("~/cogsci/projects/stanford/projects/modals/modals/experiments/80_modals_comprehension_speakerbelief/results/")
source("rscripts/helpers.r")

# load listener belief datasets
load("~/cogsci/projects/stanford/projects/modals/modals/experiments/72_modals_comprehension_evidence_room/results/data/r.RData")

## do analysis jointly on this dataset and the one from experiment 70, that differed only in  "windowless room" being replaced by "windowless office"
exp2 = r
exp2$workerid = exp2$workerid + 60
load("~/cogsci/projects/stanford/projects/modals/modals/experiments/70_modals_comprehension_evidence/results/data/r.RData")
r1 = merge(exp2, r,all=T)
r1$Experiment = "listenerbelief"
nrow(r1)

## add speakerbelief dataset for comparison of listener and speaker beliefs
load("~/cogsci/projects/stanford/projects/modals/modals/experiments/80_modals_comprehension_speakerbelief/results/data/r.RData")
r$workerid = r$workerid + 120
r$response = r$Response
r$Experiment = "speakerbelief"
both = merge(r1,r,all=T)
both$Experiment = as.factor(both$Experiment)

head(both)
summary(both)

baremust = both[both$item_type %in% c("bare","must","might"),]
baremust = droplevels(baremust)
nrow(baremust)
table(baremust$item_type)

# first analyze only speaker belief experiment
sp = droplevels(subset(baremust, Experiment == "speakerbelief"))
centered = cbind(sp, myCenter(baremust[,c("item_type","Directness","EvidenceDirectnessCategorical","Experiment")]))
table(centered$Experiment)
contrasts(centered$item_type)
head(centered)

## analysis of speaker belief after bare vs "must"
m = lmer(response~citem_type*cDirectness + (1|workerid) + (1|item), data=centered)
summary(m)

m = lmer(response~item_type + (1|workerid) + (1|item), data=centered)
summary(m)


# means
agr = aggregate(response~item_type+Experiment,FUN=mean,data=r)
agr$SD = aggregate(response~item_type+Experiment,FUN=sd,data=r)$response
agr


# analysis of both listener and speaker belief exps
centered = cbind(baremust, myCenter(baremust[,c("item_type","Directness","EvidenceDirectnessCategorical","Experiment")]))
table(centered$Experiment)
contrasts(centered$item_type)
head(centered)

## analysis of evidence after bare vs "must"
m = lmer(response~citem_type*cDirectness*cExperiment + (1|workerid) + (1|item), data=centered)
summary(m)

m = lmer(response~citem_type*cExperiment + (1|workerid) + (1|item), data=centered)
summary(m)

library(lmerTest)

