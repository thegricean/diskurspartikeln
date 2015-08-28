theme_set(theme_bw(18))
setwd("/Users/titlis/cogsci/projects/stanford/projects/diskurspartikeln/experiments/1_dp_comprehension_speakerbelief/results/")

source("rscripts/helpers.r")

load("data/r.RData")
r = read.table("data/results.txt",quote="", sep="\t", header=T)
head(r)
nrow(r)
names(r)
summary(r)
r$trial = r$slide_number_in_experiment - 2
r = r[,c("subject", "rt", "language","age","gender","trial","enjoyment","asses","comments","time_in_minutes","response","response_type","item","sentence","item_type")]
head(r)

lb = droplevels(subset(r, r$response_type=="speakerbelief"))
r = droplevels(subset(r, r$response_type=="evidence"))
nrow(lb)
nrow(r)
row.names(lb) = paste(lb$subject,lb$trial)
r$listenerbelief = as.numeric(as.character(lb[paste(r$subject,r$trial),]$response))
summary(r)

# add directness ratings
directness = read.csv("../../4_dp_priors_evidencestrength/results/data/evidence_priors.txt")
str(directness)
head(directness)
row.names(directness) = paste(directness$item,directness$evidence_id)
r$Directness = directness[paste(r$item,r$response),]$mean
r$EvidenceTypeCategorical = directness[paste(r$item,r$response),]$evidence_type
r$item_english = directness[paste(r$item,r$response),]$item_english

summary(r)
table(r$item,r$response)
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

ggplot(aes(x=time_in_minutes), data=r) +
  geom_histogram()

ggplot(aes(x=age,y=time_in_minutes,color=gender), data=unique(r[,c("subject","age","time_in_minutes","gender")])) +
  geom_point() +
  geom_smooth()

unique(r$comments)

####################

