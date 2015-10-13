library(ggplot2)
theme_set(theme_bw(18))
#setwd("~/webprojects/70_modals_comprehension_evidence/results/")
setwd("/Users/titlis/cogsci/projects/stanford/projects/diskurspartikeln/experiments/2_dp_comprehension_listenerbelief/results/")
source("rscripts/helpers.r")
load("data/r.RData")

# baremust = r[r$item_type %in% c("bare","must"),]
# baremust = droplevels(baremust)
baremust = r
nrow(baremust)
table(baremust$item_type)

m = lmer(response~item_type + (1|subject) + (1|item), data=r)
summary(m)

library(lmerTest)
m = lmer(response~item_type + (1|subject) + (1|item), data=r)
summary(m)

setwd("/Users/titlis/cogsci/projects/stanford/projects/diskurspartikeln/experiments/1_dp_comprehension_speakerbelief/results/")
source("rscripts/helpers.r")
load("data/r.RData")

# baremust = r[r$item_type %in% c("bare","must"),]
# baremust = droplevels(baremust)
baremust = r
nrow(baremust)
table(baremust$item_type)

m = lmer(response~item_type + (1|subject) + (1|item), data=r)
summary(m)

library(lmerTest)
m = lmer(response~item_type + (1|subject) + (1|item), data=r)
summary(m)

