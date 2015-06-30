library(ggplot2)
theme_set(theme_bw(18))
#setwd("~/webprojects/68_modals_freeproduction/results/")
setwd("/Users/titlis/cogsci/projects/stanford/projects/modals/modals/experiments/72_modals_comprehension_evidence_room/results/")

source("rscripts/helpers.r")

load("data/r.RData")
directness = read.csv("data/directness_of_evidence.csv")
str(directness)
head(directness)
row.names(directness) = paste(directness$domain,directness$evidencetype)

dness = read.csv("data/evidence_directness.csv")
str(dness)
head(dness)
row.names(dness) = paste(dness$domain,dness$evidencetype)

r = read.table("data/modals_results.tsv",quote="", sep="\t", header=T)
#r = read.table("data/modals_results.txt",quote="", sep="\t", header=T)
nrow(r)
names(r)
r$trial = r$slide_number_in_experiment - 2
r = r[,c("assignmentid","workerid", "rt", "sentence", "language","age","gender","trial","enjoyment","asses","comments","Answer.time_in_minutes","response","evidence","item","item_type","sentence")]
r$evidence = gsub("'","",r$evidence)
head(r)
r$Directness = directness[paste(r$item,r$evidence),]$prob
r$EvidenceTypeCategorical = directness[paste(r$item,r$evidence),]$type
r$EvidenceDirectnessCategorical = dness[paste(r$item,r$evidence),]$type

summary(r)
table(r$item,r$item_type)
save(r,file="data/r.RData")

##################

ggplot(aes(x=gender), data=r) +
  geom_histogram()

ggplot(aes(x=rt), data=r) +
  geom_histogram() +
  scale_x_continuous(limits=c(0,50000))

ggplot(aes(x=age), data=r) +
  geom_histogram()

ggplot(aes(x=age,fill=gender), data=r) +
  geom_histogram()

ggplot(aes(x=enjoyment), data=r) +
  geom_histogram()

ggplot(aes(x=asses), data=r) +
  geom_histogram()

ggplot(aes(x=Answer.time_in_minutes), data=r) +
  geom_histogram()

ggplot(aes(x=age,y=Answer.time_in_minutes,color=gender), data=unique(r[,c("assignmentid","age","Answer.time_in_minutes","gender")])) +
  geom_point() +
  geom_smooth()

unique(r$comments)

####################
