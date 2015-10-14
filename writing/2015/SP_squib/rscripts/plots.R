theme_set(theme_bw(18))
setwd("/Users/titlis/cogsci/projects/stanford/projects/diskurspartikeln/writing/2015/SP_squib/pics/")

source("../rscripts/helpers.r")

########################
# 1. EVIDENCE STRENGTH #
########################

# load english evidence strengths
load("/Users/titlis/cogsci/projects/stanford/projects/modals/modals/experiments/evidence_directness/data/d.RData")
english = d
english$Language = "English"
english$trial = english$order
english$response = english$prob
mapping = read.table("../data/evidence_id_mapping_english.txt",header=T,sep="\t",encoding="utf8",quote="")
mapping
row.names(mapping) = mapping$evidence
english$evidence_id = mapping[as.character(english$evidence),]$evidence_id
table(english$domain,english$evidence_id) # how many ratings for each piece of evidence?

# load german evidence strengths
load("/Users/titlis/cogsci/projects/stanford/projects/diskurspartikeln/experiments/4_dp_priors_evidencestrength/results/data/r.RData")
german = r
german$workerID = as.numeric(as.character(german$subject)) + 40
german$domain = as.factor(ifelse(german$item == "abendessen","dinner",ifelse(german$item == "kaffee", "coffee", ifelse(german$item == "hund","dog","rain"))))
german$Language = "German"
table(german$domain,german$evidence_id) # how many ratings for each piece of evidence?

# how many participants in each?
length(levels(as.factor(as.character(english$workerID)))) #40
length(levels(as.factor(as.character(german$workerID)))) #39 -- why one less??

# merge datasets
nrow(english)
nrow(german)
d = merge(english, german, all=T) %>%
  select(workerID, domain, evidence, evidence_id, Language, trial, response)
nrow(d)
summary(d)
d$workerID = as.factor(as.character(d$workerID))
d$Language = as.factor(as.character(d$Language))

# plot histograms side by side
ggplot(d, aes(x=response)) +
  geom_histogram() +
  facet_wrap(~Language)

# plot histograms of mean evidence strengths side by side, first aggregating by item
agr = d %>%
  group_by(Language,domain,evidence_id) %>%
  summarise(mean=mean(response),ci.low=ci.low(response),ci.high=ci.high(response))
agr = as.data.frame(agr)
agr

ggplot(agr, aes(x=mean)) +
  geom_histogram(binwidth=.05) +
  facet_wrap(~Language) +
  scale_x_continuous(name="Mean evidence strength") +
  scale_y_continuous(name="Number of cases")  
ggsave("evidencestrength-histograms.pdf",height=3.5,width=7)

# alternative: plot mean evidence strength for each item; english and german side by side
dodge = position_dodge(.9)
agr$YMin = agr$mean - agr$ci.low
agr$YMax = agr$mean + agr$ci.high

ggplot(agr, aes(x=evidence_id,y=mean,fill=Language)) +
  geom_bar(stat="identity",position=dodge) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~domain) +
  scale_x_discrete(name="Piece of evidence") +
  scale_y_continuous(name="Mean strength")  
ggsave("mean-evidencestrength-byitem.pdf",height=6.5,width=9)

# analysis: do the two distributions differ? nope
d$item = as.factor(paste(d$domain,d$evidence_id))
m = lmer(response ~ Language + (1|workerID) + (1+Language|item), data=d)
summary(m)
library(lmerTest)

#################
# 2. PRODUCTION #
#################

# load english production data
load("/Users/titlis/cogsci/projects/stanford/projects/modals/modals/experiments/71_modals_forced_production/results/data/r.RData")
english_p = r
english_p$Language = "English"

head(english_p)
nrow(english_p)

# load german production data
load("/Users/titlis/cogsci/projects/stanford/projects/diskurspartikeln/experiments/3_dp_production/results/data/r.RData")
german_p = r
german_p$Language = "German"
german_p$workerid = german_p$subject + 40
german_p$item = german_p$item_english

# merge datasets
d = merge(english_p, german_p, all=T)  %>%
  select(workerid, item, Language, evidence_type, trial, response, EvidenceTypeCategorical, Directness)
nrow(d)
d$workerid = as.factor(as.character(d$workerid))
d$Language = as.factor(as.character(d$Language))
summary(d)
nrow(d)

# how many participants in each?
length(levels(as.factor(as.character(english_p$workerid)))) #40
length(levels(as.factor(as.character(german_p$subject)))) #38 -- why 2 less?

# plot overall distribution of utterances
d$Utterance = factor(x=d$response,levels=c("bare","must","muss","probably","might","wohl","vermutlich"))
ggplot(d, aes(x=Utterance)) +
  geom_histogram() +
  facet_wrap(~Language, scales="free") +
  scale_y_continuous("Number of cases")
ggsave("production-distribution.pdf",width=7,height=4)

# plot mean evidence strength by utterance
agr = d %>%
  group_by(Language,response) %>%
  summarise(mean=mean(Directness),ci.low=ci.low(Directness),ci.high=ci.high(Directness))
agr = as.data.frame(agr)
head(agr)

dodge = position_dodge(.9)
agr$YMin = agr$mean - agr$ci.low
agr$YMax = agr$mean + agr$ci.high
agr$Utterance = factor(x=agr$response,levels=c("bare","must","muss","probably","might","wohl","vermutlich"))

ggplot(agr, aes(x=Utterance,y=mean)) +
  geom_bar(stat="identity",position=dodge,fill="gray70",color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  facet_wrap(~Language,scales="free") +
  scale_x_discrete(name="Utterance") +
  scale_y_continuous(name="Mean evidence strength") #+
  #scale_fill_manual(values=c("white","gray70"))
ggsave("mean-production-evidence.pdf",height=4,width=7)

english = droplevels(subset(d, Language == "English"))
contrasts(english$response) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
english$complete_item = as.factor(paste(english$item,english$evidence_type))
m = lmer(Directness ~ response + (1|workerid) + (1|item), data = english)
summary(m)

german = droplevels(subset(d, Language == "German"))
contrasts(german$response) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
german$complete_item = as.factor(paste(german$item,german$evidence_type))
m = lmer(Directness ~ response + (1|workerid) + (1|item), data = german)
summary(m)


######################
# 3AB. COMPREHENSION #
######################

# load english listener belief data
load("/Users/titlis/cogsci/projects/stanford/projects/modals/modals/experiments/72_modals_comprehension_evidence_room/results/data/r.RData")
english_cl = r
english_cl$Language = "English"
english_cl$Belief = "listener"
head(english_cl)

# load english speaker belief data
load("/Users/titlis/cogsci/projects/stanford/projects/modals/modals/experiments/80_modals_comprehension_speakerbelief/results/data/r.RData")
english_cs = r
english_cs$response = english_cs$Response
english_cs$Language = "English"
english_cs$Belief = "speaker"
english_cs$workerid = english_cs$workerid + 60 # adjust so no workerid overlap
head(english_cs)

# load german listener belief data
load("/Users/titlis/cogsci/projects/stanford/projects/diskurspartikeln/experiments/2_dp_comprehension_listenerbelief/results/data/r.RData")
german_cl = r
german_cl$Language = "German"
german_cl$Belief = "listener"
german_cl$item = german_cl$item_english
german_cl$workerid = german_cl$subject + 120
head(german_cl)

# load german speaker belief data
load("/Users/titlis/cogsci/projects/stanford/projects/diskurspartikeln/experiments/1_dp_comprehension_speakerbelief/results/data/r.RData")
german_cs = r
german_cs$Language = "German"
german_cs$Belief = "speaker"
german_cs$item = german_cs$item_english
german_cs$workerid = german_cs$subject + 180
head(german_cs)

# how many participants in each?
length(levels(as.factor(as.character(english_cl$workerid)))) #60
length(levels(as.factor(as.character(english_cs$workerid)))) #60
length(levels(as.factor(as.character(german_cl$subject)))) #56 -- why four less??
length(levels(as.factor(as.character(german_cs$subject)))) # 62

# merge datasets
nrow(english_cl)
nrow(english_cs)
nrow(german_cl)
nrow(german_cs)
english = merge(english_cl,english_cs, all=T)
english$item_type = as.character(english$item_type)
german = merge(german_cl,german_cs, all=T)
german$item_type = as.character(german$item_type)
german$evidence = german$response
german$response = german$listenerbelief
nrow(english)
nrow(german)
d = merge(english, german, all=T) %>%
  select(workerid, item, item_type, Language, trial, response, evidence, Belief, Directness)
nrow(d)
d$workerid = as.factor(as.character(d$workerid))
d$Language = as.factor(as.character(d$Language))
d$evidence = as.factor(as.character(d$evidence))
d$Belief = as.factor(as.character(d$Belief))
d$item_type = as.factor(as.character(d$item_type))
summary(d)

save(d, file="../data/d_comprehension.RData")

# plot mean belief by utterance
agr = d %>%
  group_by(Language,Belief,item_type) %>%
  summarise(mean=mean(response),ci.low=ci.low(response),ci.high=ci.high(response))
agr = as.data.frame(agr)
head(agr)

dodge = position_dodge(.9)
agr$YMin = agr$mean - agr$ci.low
agr$YMax = agr$mean + agr$ci.high
agr$Utterance = factor(x=agr$item_type,levels=c("bare","must","muss","probably","might","wohl","vermutlich"))
agr$Belief = as.factor(ifelse(agr$Belief == "listener","listener\n(Exps. 3a)","speaker\n(Exps. 3b)"))

ggplot(agr, aes(x=Utterance,y=mean,fill=Belief)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",position=dodge,color="black",show_guide=F) +  
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~Language,scales="free") +
  scale_x_discrete(name="Utterance") +
  scale_y_continuous(name="Mean degree of belief") +
  scale_fill_manual(values=c("white","gray70"))
ggsave("mean-beliefs.pdf",height=3.5,width=7.5)

english = droplevels(subset(d, Language == "English" & Belief == "listener"))
contrasts(english$item_type) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
m = lmer(response ~ item_type + (1|workerid) + (1|item), data = english)
summary(m)

english = droplevels(subset(d, Language == "English" & Belief == "speaker"))
contrasts(english$item_type) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
m = lmer(response ~ item_type + (1|workerid) + (1|item), data = english)
summary(m)

english = droplevels(subset(d, Language == "English"))
contrasts(english$item_type) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
m = lmer(response ~ item_type*Belief + (1|workerid) + (1|item), data = english)
summary(m)

german = droplevels(subset(d, Language == "German" & Belief == "listener"))
contrasts(german$item_type) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
m = lmer(response ~ item_type + (1|workerid) + (1|item), data = german)
summary(m)

german = droplevels(subset(d, Language == "German" & Belief == "speaker"))
contrasts(german$item_type) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
m = lmer(response ~ item_type + (1|workerid) + (1|item), data = german)
summary(m)

german = droplevels(subset(d, Language == "German"))
contrasts(german$item_type) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
m = lmer(response ~ item_type*Belief + (1|workerid) + (1|item), data = german)
summary(m)

# plot mean inferred evidence strength by utterance
agr = d %>%
  group_by(Language,Belief,item_type) %>%
  summarise(mean=mean(Directness),ci.low=ci.low(Directness),ci.high=ci.high(Directness))  
agr = as.data.frame(agr)
head(agr)

dodge = position_dodge(.9)
agr$YMin = agr$mean - agr$ci.low
agr$YMax = agr$mean + agr$ci.high
agr$Utterance = factor(x=agr$item_type,levels=c("bare","must","muss","probably","might","wohl","vermutlich"))
agr$Belief = as.factor(ifelse(agr$Belief == "listener","listener\n(Exps. 3a)","speaker\n(Exps. 3b)"))

ggplot(agr, aes(x=Utterance,y=mean,fill=Belief)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",position=dodge,color="black",show_guide=F) +  
  geom_errorbar(aes(ymin=YMin,ymax=YMax),position=dodge,width=.25) +
  facet_wrap(~Language,scales="free") +
  scale_x_discrete(name="Utterance") +
  scale_y_continuous(name="Mean strength of inferred evidence") +
  scale_fill_manual(values=c("white","gray70"))
ggsave("mean-evidence.pdf",height=3.5,width=7.5)

english = droplevels(subset(d, Language == "English" & Belief == "listener"))
contrasts(english$item_type) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
m = lmer(Directness ~ item_type + (1|workerid) + (1|item), data = english)
summary(m)

english = droplevels(subset(d, Language == "English" & Belief == "speaker"))
contrasts(english$item_type) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
m = lmer(Directness ~ item_type + (1|workerid) + (1|item), data = english)
summary(m)

english = droplevels(subset(d, Language == "English"))
contrasts(english$item_type) = cbind("bare.vs.must"=c(1,0,0,0),"might.vs.must"=c(0,1,0,0),"probably.vs.must"=c(0,0,0,1))
m = lmer(Directness ~ item_type*Belief + (1|workerid) + (1|item), data = english)
summary(m)

german = droplevels(subset(d, Language == "German" & Belief == "listener"))
contrasts(german$item_type) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
m = lmer(Directness ~ item_type + (1|workerid) + (1|item), data = german)
summary(m)

german = droplevels(subset(d, Language == "German" & Belief == "speaker"))
contrasts(german$item_type) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
m = lmer(Directness ~ item_type + (1|workerid) + (1|item), data = german)
summary(m)

german = droplevels(subset(d, Language == "German"))
contrasts(german$item_type) = cbind("bare.vs.muss"=c(1,0,0,0),"vermutlich.vs.muss"=c(0,0,1,0),"wohl.vs.muss"=c(0,0,0,1))
m = lmer(Directness ~ item_type*Belief + (1|workerid) + (1|item), data = german)
summary(m)

# histogram of inferred evidence pieces
ggplot(d, aes(x=evidence)) +
  geom_histogram() +
  facet_grid(Language~item_type)

